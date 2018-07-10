/*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */

#include <errno.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mcheck.h>

#include "unixpwd.h"

int
main(int argc, char **argv)
{
    int             rc;
    char           *pwd;
    char           *buf;
    char           *msg;
    int             i;

    mtrace();
    switch (argc) {
    case 1:
        for (i = 0; i < 100; i++) {
            buf = unixpwd_unshadow();
            msg = strerror(errno);
            if (buf) {
                free(buf);
            } else {
                fprintf(stderr, "can't unshadow: %s\n", msg);
                break;
            }
        }
        buf = unixpwd_unshadow();
        msg = strerror(errno);
        if (buf) {
            puts(buf);
            free(buf);
        } else {
            fprintf(stderr, "can't unshadow: %s\n", msg);
            break;
        }
        break;

    case 2:
        pwd = unixpwd_get(argv[1]);
        msg = strerror(errno);
        if (pwd) {
            printf("%s: %s\n", argv[1], pwd);
            free(pwd);
            rc = 0;
        } else {
            fprintf(stderr, "can't find entry for %s: %s\n", argv[1], msg);
            rc = 1;
        }

        pwd = unixpwd_getpwd(argv[1]);
        msg = strerror(errno);
        if (pwd) {
            printf("/etc/passwd: %s: %s\n", argv[1], pwd);
            free(pwd);
            rc = 0;
        } else {
            fprintf(stderr, "can't find passwd entry for %s: %s\n",
                    argv[1], msg);
            rc = 1;
        }

        pwd = unixpwd_getspw(argv[1]);
        msg = strerror(errno);
        if (pwd) {
            printf("/etc/shadow: %s: %s\n", argv[1], pwd);
            free(pwd);
            rc = 0;
        } else {
            fprintf(stderr, "can't find shadow entry for %s: %s\n",
                    argv[1], msg);
            rc = 1;
        }

        break;

    case 3:
        rc = unixpwd_setpwd(argv[1], argv[2]);
        msg = strerror(errno);
        if (rc != 0) {
            fprintf(stderr, "error setting password: %s\n", msg);
            rc = 1;
            break;
        }
        rc = unixpwd_setspw(argv[1], argv[2]);
        msg = strerror(errno);
        if (rc != 0) {
            fprintf(stderr, "error setting shadow password: %s\n", msg);
            rc = 1;
            break;
        }
        break;

    default:
        fprintf(stderr, "usage: unixpwd [user [password]]\n");
        rc = 1;
    }
    exit(rc);
}
