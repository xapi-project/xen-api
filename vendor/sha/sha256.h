/*
 	Copyright (C) 2006-2009 Vincent Hanquez <tab@snarc.org>
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
 * SHA256 implementation
 */
#ifndef SHA256_H
#define SHA256_H

struct sha256_ctx
{
	unsigned int h[8];
	unsigned char buf[128];
	unsigned long long sz;
};

typedef struct { unsigned int digest[8]; } sha256_digest;

void sha256_init(struct sha256_ctx *ctx);
void sha256_copy(struct sha256_ctx *dst, struct sha256_ctx *src);
void sha256_update(struct sha256_ctx *ctx, unsigned char *data, int len);
void sha256_finalize(struct sha256_ctx *ctx, sha256_digest *out);
void sha256_to_bin(sha256_digest *digest, char *out);
void sha256_to_hex(sha256_digest *digest, char *out);

#endif
