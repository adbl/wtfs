#ifndef _WTFS_DRV_H
#define _WTFS_DRV_H

#define FUSE_USE_VERSION 26

#include "erl_driver.h"

typedef struct {
    ErlDrvPort port;
    struct fuse_chan* channel;
    struct fuse_session* session;
    char* mountpoint;
} port_data;


#endif
