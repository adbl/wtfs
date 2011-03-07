#define FUSE_USE_VERSION 26

#include <stdio.h>
#include <errno.h>
#include <fuse_lowlevel.h>
#include "erl_driver.h"

#include "wtfs_drv.h"

static void op_lookup(fuse_req_t req, fuse_ino_t parent, const char *name) {
    printf("lookup: %s\n", name);
    fuse_reply_err(req, 1);
}

static void op_getattr(fuse_req_t req, fuse_ino_t ino,
                       struct fuse_file_info *fi) {
    printf("getattr: %u\n", (int) ino);
    fuse_reply_err(req, 1);
}

static void op_open(fuse_req_t req, fuse_ino_t ino, struct fuse_file_info *fi) {
    printf("open: %u\n", (int) ino);
    fuse_reply_err(req, 1);
}

static void op_read(fuse_req_t req, fuse_ino_t ino, size_t size, off_t off,
                    struct fuse_file_info *fi) {
    printf("read: %u\n", (int) ino);
    fuse_reply_err(req, 1);
}

static void op_readdir(fuse_req_t req, fuse_ino_t ino, size_t size, off_t off,
                       struct fuse_file_info *fi) {
    printf("readdir: %u\n", (int) ino);
    fuse_reply_err(req, 1);
}

const struct fuse_lowlevel_ops ops = {
    /* op_init, */
    /* op_destroy, */
    .lookup  = op_lookup,
    /* op_forget, */
    .getattr = op_getattr,
    /* op_setattr, */
    /* op_readlink, */
    /* op_mknod, */
    /* op_mkdir, */
    /* op_unlink, */
    /* op_rmdir, */
    /* op_symlink, */
    /* op_rename, */
    /* op_link, */
    .open    = op_open,
    .read    = op_read,
    /* op_write, */
    /* op_flush, */
    /* op_release, */
    /* op_fsync, */
    /* op_opendir, */
    .readdir = op_readdir,
    /* op_releasedir, */
    /* op_fsyncdir, */
    /* op_statfs, */
    /* op_setxattr, */
    /* op_getxattr, */
    /* op_listxattr, */
    /* op_removexattr, */
    /* op_access, */
    /* op_create, */
    /* op_getlk, */
    /* op_setlk, */
    /* op_bmap, */
    /* op_ioctl, */
    /* op_poll */
};

/* This is called when an erlang process has sent data to the port. */
static void port_output(ErlDrvData handle, char* buf, int len) {
    printf("port_output: %s\n", buf);
}

/* This is called when a driver event (given in the event parameter) is
   signaled. */
static void port_ready_input(ErlDrvData handle, ErlDrvEvent event) {
    printf("port_ready_input\n");
    port_data* data = (port_data*) handle;

    int res = 0;
    /* /\* Iterate over the channels assigned to a session *\/ */
    /* struct fuse_chan *ch = fuse_session_next_chan(data->session, NULL); */

    /* Query the exited status of a session */
    /* while (!fuse_session_exited(se)) {} */

    struct fuse_chan *tmpch = data->channel;
    /* Receive a raw request */
    res = fuse_chan_recv(&tmpch, data->read_buffer, data->read_buffer_size);
    if (res > 0) {
        /* Process a raw request */
        fuse_session_process(data->session, data->read_buffer, res, tmpch);
    }
    else {
        fprintf(stderr, "fuse_chan_recv returned %d\n", res);
        driver_failure(data->port, res);
        /* ENODEV instead of fuse_session_exited? */
        /* if (res == -EINTR) */
        /*     continue; */
        /* if (res <= 0) */
        /*     break; */
    }

    /* Reset the exited status of a session */
    /* fuse_session_reset(se); */
    /* return res < 0 ? -1 : 0; */
}

int init_buffer(port_data* data) {
    printf("init_buffer\n");
    /* Query the minimal receive buffer size */
    size_t size = fuse_chan_bufsize(data->channel);
    char* buffer = (char *) driver_alloc(size);
    if (buffer != NULL) {
        data->read_buffer = buffer;
        data->read_buffer_size = size;
        return 0;
    }
    else {
        fprintf(stderr, "failed to allocate read buffer\n");
    }
    return -1;
}

int init_event(port_data* data) {
    printf("init_event\n");
    ErlDrvEvent event = (ErlDrvEvent) (intptr_t) fuse_chan_fd(data->channel);
    /* ERL_DRV_USE should be set together with the first event,
       in port_ready_input? */
    if (driver_select(data->port, event, ERL_DRV_READ | ERL_DRV_USE, 1) != -1) {
        data->channel_fd = event;
        if (init_buffer(data) != -1) {
            return 0;
        }
        else {
            /* clear all events (and wait for port_stop_select callback?) */
            driver_select(data->port, data->channel_fd, ERL_DRV_USE, 0);
            /* stop listening for events */
            driver_select(data->port, data->channel_fd, ERL_DRV_READ, 0);
        }
    }
    else {
        fprintf(stderr, "failed to listen for events\n");
    }
    return -1;
}

int init_signals(port_data* data) {
    printf("init_signals\n");
    /* Exit session on HUP, TERM and INT signals and ignore PIPE */
    if (fuse_set_signal_handlers(data->session) != -1) {
        if (init_event(data) != -1) {
            return 0;
        }
        else {
            /* Restore default signal handlers */
            fuse_remove_signal_handlers(data->session);
        }
    }
    else {
        fprintf(stderr, "failed to set up signal handlers\n");
    }
    return -1;
}

int init_session(struct fuse_args* args, port_data* data) {
    printf("init_session\n");
    /* Create a lowlevel session */
    struct fuse_session* session;
    session = fuse_lowlevel_new(args, &ops, sizeof(ops), NULL);
    if (session != NULL) {
        data->session = session;
        /* Assign a channel to a session */
        fuse_session_add_chan(session, data->channel);
        if (init_signals(data) != -1) {
            return 0;
        }
        else {
            /* Destroy a session (and assigned channel) */
            fuse_session_destroy(session);
        }
    }
    else {
        fprintf(stderr, "failed to create fuse session\n");
    }
    return -1;
}

int init_channel(struct fuse_args* args, char* mountpoint, port_data* data) {
    printf("init_channel\n");
    /* Create a FUSE mountpoint communication channel */
    struct fuse_chan* channel = fuse_mount(mountpoint, args);
    if (channel != NULL) {
        data->channel = channel;
        if (init_session(args, data) != -1) {
            return 0;
        }
        else {
            /* Umount a FUSE mountpoint */
            fuse_unmount(mountpoint, channel);
        }
    }
    else {
        fprintf(stderr, "failed to create fuse mountpoint channel\n");
    }
    return -1;
}

int init_args(int argc, char* argv[], port_data* data) {
    printf("init_args\n");
    int ret = -1;
    struct fuse_args args = FUSE_ARGS_INIT(argc, argv);
    char* mountpoint;
    /* parse args and set mountpoint string (should be freed) */
    /* TODO: "bad mount point `...': Transport endpoint is not connected" */
    if (fuse_parse_cmdline(&args, &mountpoint, NULL, NULL) != -1) {
        data->mountpoint = mountpoint;
        if (init_channel(&args, mountpoint, data) != -1) {
            ret = 0;
        }
        else {
            driver_free(mountpoint);
        }
    }
    else {
        fprintf(stderr, "failed to parse fuse arguments\n");
    }
    /* Free the contents of argument list */
    fuse_opt_free_args(&args);
    return ret;
}

/* This is called when the driver is instantiated */
static ErlDrvData port_start(ErlDrvPort port, char* command) {
    printf("port_start: %s\n", command);
    /* TODO: use command to get mountpoint */

    port_data* data = (port_data*) driver_alloc(sizeof(port_data));
    data->port = port;
    int argc = 2;
    char* argv[] = {"wtfs_drv", "/tmp/fuse"};

    if (init_args(argc, argv, data) != -1) {
        return (ErlDrvData) data;
    }
    else {
        return ERL_DRV_ERROR_GENERAL;
    }
}

static void port_stop(ErlDrvData handle) {
    printf("port_stop\n");
    port_data* data = (port_data*) handle;
    /* clear all events (and wait for port_stop_select callback?) */
    driver_select(data->port, data->channel_fd, ERL_DRV_USE, 0);
    /* stop listening for events */
    driver_select(data->port, data->channel_fd, ERL_DRV_READ, 0);
    /* Restore default signal handlers */
    fuse_remove_signal_handlers(data->session);
    /* Remove a channel from a session */
    fuse_session_remove_chan(data->channel);
    /* Destroy a session */
    fuse_session_destroy(data->session);
    /* Umount a FUSE mountpoint */
    fuse_unmount(data->mountpoint, data->channel);
    /* fuse_chan_destroy(data->channel); */

    /* driver_free(data->mountpoint); */
    driver_free(data->read_buffer);
    driver_free((char*) handle);
}

static void port_stop_select(ErlDrvEvent event, void* reserved) {
    printf("port_stop_select\n");
}

ErlDrvEntry driver_entry = {
    NULL,              /* int (*init)(void) */
    port_start,        /* ErlDrvData (*start)(ErlDrvPort port, char *command) */
    port_stop,         /* void (*stop)(ErlDrvData drv_data) */
    port_output,       /* void (*output)(ErlDrvData drv_data, char *buf,
                                         int len) */
    port_ready_input,  /* void (*ready_input)(ErlDrvData drv_data,
                                              ErlDrvEvent event) */
    NULL,              /* void (*ready_output)(ErlDrvData drv_data,
                                               ErlDrvEvent event) */
    "wtfs_drv",        /* char *driver_name */
    NULL,              /* void (*finish)(void) */
    NULL,              /* reserved */
    NULL,              /* int (*control)(ErlDrvData drv_data,
                                         unsigned int command, char *buf,
                                         int len, char **rbuf, int rlen) */
    NULL,              /* void (*timeout)(ErlDrvData drv_data) */
    NULL,              /* void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev); */
    NULL,              /* void (*ready_async)(ErlDrvData drv_data,
                                              ErlDrvThreadData thread_data); */
    NULL,              /* void (*flush)(ErlDrvData drv_data); */
    NULL,              /* int (*call)(ErlDrvData drv_data, unsigned int command,
                                      char *buf, int len, char **rbuf, int rlen,
                                      unsigned int *flags) */
    NULL,              /* "intentionally left undocumented" */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,                /* driver flags */
    NULL,             /* reserved */
    NULL,             /* void (*process_exit)(ErlDrvData drv_data,
                                              ErlDrvMonitor *monitor) */
    port_stop_select  /* void (*stop_select)(ErlDrvEvent event,
                                             void* reserved) */
};

DRIVER_INIT(wtfs_drv) /* must match name in driver_entry */
{
    return &driver_entry;
}
