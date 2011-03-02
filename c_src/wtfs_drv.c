#define FUSE_USE_VERSION 26

#include <fuse_lowlevel.h>
#include <stdio.h>
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

const struct fuse_lowlevel_ops operations = {
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
    /* Iterate over the channels assigned to a session */
    struct fuse_chan *ch = fuse_session_next_chan(data->session, NULL);
    /* Query the minimal receive buffer size */
    size_t bufsize = fuse_chan_bufsize(ch);
    char *buf = (char *) driver_alloc(bufsize);
    if (!buf) {
        fprintf(stderr, "fuse: failed to allocate read buffer\n");
    /*     return -1; */
    }
    else {
        fprintf(stderr, "fuse: allocated MEMORY\n");
    }

    /* Query the exited status of a session */
    /* while (!fuse_session_exited(se)) { */
        struct fuse_chan *tmpch = ch;
        /* Receive a raw request */
        res = fuse_chan_recv(&tmpch, buf, bufsize);
        /* if (res == -EINTR) */
        /*     continue; */
        /* if (res <= 0) */
        /*     break; */
        /* Process a raw request */
        fprintf(stderr, "received!\n");
        fuse_session_process(data->session, buf, res, tmpch);
        fprintf(stderr, "processed!\n");
    /* } */

    driver_free(buf);
    /* Reset the exited status of a session */
    /* fuse_session_reset(se); */
    /* return res < 0 ? -1 : 0; */
}

/* This is called when the driver is instantiated */
static ErlDrvData port_start(ErlDrvPort port, char* command) {
    printf("port_start: %s\n", command);
    /* TODO: use command to get mountpoint */

    port_data* data = (port_data*) driver_alloc(sizeof(port_data));
    data->port = port;
    int argc = 2;
    char* argv[] = {"wtfs", "/tmp/fuse"};

    struct fuse_args args = FUSE_ARGS_INIT(argc, argv);
    struct fuse_chan* channel;
    char* mountpoint;

    /* parse args and set mountpoint string (should be freed) */
    if (fuse_parse_cmdline(&args, &mountpoint, NULL, NULL) != -1 &&
        /* Create a FUSE mountpoint communication channel */
        (channel = fuse_mount(mountpoint, &args)) != NULL) {
        struct fuse_session* session;
        /* Create a lowlevel session */
        session = fuse_lowlevel_new(&args, &operations,
                                    sizeof(operations), NULL);
        if (session != NULL) {
            /* Exit session on HUP, TERM and INT signals and ignore PIPE */
            if (fuse_set_signal_handlers(session) != -1) {
                /* Assign a channel to a session */
                fuse_session_add_chan(session, channel);

                ErlDrvEvent event =
                    (ErlDrvEvent) (intptr_t) fuse_chan_fd(channel);
                printf("fuse_chan_df: %d\n", (int) event);
                if (driver_select(port, event, ERL_DRV_READ | ERL_DRV_USE, 1)
                    == 0) {
                    /* ERL_DRV_USE should be set together with the first
                       event, in port_ready_input? */
                    data->mountpoint = mountpoint;
                    data->channel = channel;
                    data->session = session;
                    return (ErlDrvData) data;
                }
            }
            /* Destroy a session */
            fuse_session_destroy(session);
        }
        /* Umount a FUSE mountpoint */
        fuse_unmount(mountpoint, channel);
    }
    /* Free the contents of argument list, the structure itself is not freed */
    fuse_opt_free_args(&args);

    /* ERL_DRV_ERROR_GENERAL
       ERL_DRV_ERROR_ERRNO
       ERL_DRV_ERROR_BADARG */
    return ERL_DRV_ERROR_GENERAL;
}

static void port_stop(ErlDrvData handle) {
    printf("port_stop\n");
    port_data* data = (port_data*) handle;
    /* clear all events (and wait for port_stop_select callback?) */
    driver_select(data->port, data->event, ERL_DRV_USE, 0);
    /* stop listening for events */
    driver_select(data->port, data->event, ERL_DRV_READ, 0);
    /* Restore default signal handlers */
    fuse_remove_signal_handlers(data->session);
    /* Remove a channel from a session */
    fuse_session_remove_chan(data->channel);
    /* Destroy a session */
    fuse_session_destroy(data->session);
    /* Umount a FUSE mountpoint */
    fuse_unmount(data->mountpoint, data->channel);

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
