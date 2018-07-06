/*
 *	Copyright (C) 2006-2009 Vincent Hanquez <tab@snarc.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * SHA512 implementation
 */
#ifndef SHA512_H
#define SHA512_H

#include <stdint.h>

struct sha512_ctx
{
	uint64_t h[8];
	unsigned char buf[128];
	uint64_t sz[2];
};

typedef struct { uint64_t digest[8]; } sha512_digest;

void sha512_init(struct sha512_ctx *ctx);
void sha512_copy(struct sha512_ctx *dst, struct sha512_ctx *src);
void sha512_update(struct sha512_ctx *ctx, unsigned char *data, int len);
void sha512_finalize(struct sha512_ctx *ctx, sha512_digest *out);
void sha512_to_bin(sha512_digest *digest, char *out);
void sha512_to_hex(sha512_digest *digest, char *out);

#endif
