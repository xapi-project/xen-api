/*
 * Copyright (c) 2007 XenSource Inc.
 * Author Vincent Hanquez <vincent@xensource.com>
 *
 * All rights reserved.
 */
#ifndef FAKE_USING_H
#define FAKE_USING_H

static int using_injection(void)
{
	return (getenv("XIU") != NULL);
}

#endif /* !FAKE_USING_H */
