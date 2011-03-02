#ifndef _WTFS_DRV_H
#define _WTFS_DRV_H

#define FUSE_USE_VERSION 26

#include "erl_driver.h"

typedef struct {
    ErlDrvPort port;
    char* mountpoint;
    struct fuse_chan* channel;
    struct fuse_session* session;
    ErlDrvEvent event;
} port_data;


#endif
