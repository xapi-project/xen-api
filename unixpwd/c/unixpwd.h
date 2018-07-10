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

/*
 * get password for user. The result must be passed to free(). On error,
 * returns NULL and errno set. unixpwd_get tries to obtain the shadow
 * password first and if that fails to obtain the password from
 * /etc/passwd. unixpwd_getpwd() obtains the passowrd from /etc/passwd
 * and unixpwd_getspw() obtains the password from /etc/shadow.
 */

char           *unixpwd_get(const char *user);
char           *unixpwd_getpwd(const char *user);
char           *unixpwd_getspw(const char *user);
/*
 * return /etc/passwd as a string but with entries from shadow passwords
 * when they exist. The returned string must be passed to free(). On
 * error, returns NULL and errno set.
 */

char           *unixpwd_unshadow(void);

/*
 * update password for user in /etc/passwd and /etc/shadow respectively
 * and return 0 on success and errno otherwise. Specific errors: EINVAL:
 * no password entry for user exists ENOLCK: can't acquire lock for
 * unixpwd_setspw
 */

int             unixpwd_setpwd(const char *user, char *password);
int             unixpwd_setspw(const char *user, char *password);
