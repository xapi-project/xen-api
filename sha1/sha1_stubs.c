/*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <byteswap.h>

#ifdef WORDS_BIGENDIAN
#define be16_to_cpu(x) (x)
#define be32_to_cpu(x) (x)
#define be64_to_cpu(x) (x)
#define le16_to_cpu(x) bswap_16(x)
#define le32_to_cpu(x) bswap_32(x)
#define le64_to_cpu(x) bswap_64(x)
#else
#define be16_to_cpu(x) bswap_16(x)
#define be32_to_cpu(x) bswap_32(x)
#define be64_to_cpu(x) bswap_64(x)
#define le16_to_cpu(x) (x)
#define le32_to_cpu(x) (x)
#define le64_to_cpu(x) (x)
#endif

struct sha1_ctx
{
	unsigned int state[5];
	unsigned char buf[64];
	unsigned long long count;
};

typedef struct { unsigned int digest[5]; } sha1_digest;

static void sha1_init(struct sha1_ctx *ctx)
{
	memset(ctx, 0, sizeof(*ctx));

	/* initialize H */
	ctx->state[0] = 0x67452301;
	ctx->state[1] = 0xEFCDAB89;
	ctx->state[2] = 0x98BADCFE;
	ctx->state[3] = 0x10325476;
	ctx->state[4] = 0xC3D2E1F0;
}

#define rol(value, bits) (((value) << (bits)) | ((value) >> (32 - (bits))))

/* (R0+R1), R2, R3, R4 are the different operations used in SHA1 */
#define blk0(i) (block[i] = be32_to_cpu(((unsigned int*)buffer)[i]))
#define blk(i) (block[i] = rol(block[i-3]^block[i-8]^block[i-14]^block[i-16],1))

#define R0(v,w,x,y,z,i) z+=((w&(x^y))^y)    +blk0(i)+0x5A827999+rol(v,5);w=rol(w,30);
#define R1(v,w,x,y,z,i) z+=((w&(x^y))^y)    +blk (i)+0x5A827999+rol(v,5);w=rol(w,30);
#define R2(v,w,x,y,z,i) z+=( w^x     ^y)    +blk (i)+0x6ED9EBA1+rol(v,5);w=rol(w,30);
#define R3(v,w,x,y,z,i) z+=(((w|x)&y)|(w&x))+blk (i)+0x8F1BBCDC+rol(v,5);w=rol(w,30);
#define R4(v,w,x,y,z,i) z+=( w^x     ^y)    +blk (i)+0xCA62C1D6+rol(v,5);w=rol(w,30);


static void sha1_transform(unsigned int state[5], unsigned char buffer[64])
{
	unsigned int block[80];
	unsigned int i, a, b, c, d, e;

	a = state[0];
	b = state[1];
	c = state[2];
	d = state[3];
	e = state[4];

	for (i = 0; i < 15; i += 5) {
		R0(a, b, c, d, e, 0 + i);
		R0(e, a, b, c, d, 1 + i);
		R0(d, e, a, b, c, 2 + i);
		R0(c, d, e, a, b, 3 + i);
		R0(b, c, d, e, a, 4 + i);
	}

	R0(a, b, c, d, e, 15);
	R1(e, a, b, c, d, 16);
	R1(d, e, a, b, c, 17);
	R1(c, d, e, a, b, 18);
	R1(b, c, d, e, a, 19);

	for (i = 20; i < 40; i += 5) {
		R2(a, b, c, d, e, 0 + i);
		R2(e, a, b, c, d, 1 + i);
		R2(d, e, a, b, c, 2 + i);
		R2(c, d, e, a, b, 3 + i);
		R2(b, c, d, e, a, 4 + i);
	}
	for (; i < 60; i += 5) {
		R3(a, b, c, d, e, 0 + i);
		R3(e, a, b, c, d, 1 + i);
		R3(d, e, a, b, c, 2 + i);
		R3(c, d, e, a, b, 3 + i);
		R3(b, c, d, e, a, 4 + i);
	}
	for (; i < 80; i += 5) {
		R4(a, b, c, d, e, 0 + i);
		R4(e, a, b, c, d, 1 + i);
		R4(d, e, a, b, c, 2 + i);
		R4(c, d, e, a, b, 3 + i);
		R4(b, c, d, e, a, 4 + i);
	}

	state[0] += a;
	state[1] += b;
	state[2] += c;
	state[3] += d;
	state[4] += e;
}

static void sha1_update(struct sha1_ctx *ctx, unsigned char *data, int len)
{
	unsigned int i, j;
	j = ctx->count & 63;
	ctx->count += len;

	if ((j + len) > 63) {
		i = 64 - j;
		memcpy(&ctx->buf[j], data, i);
		sha1_transform(ctx->state, ctx->buf);
		for ( ; i + 63 < len; i += 64) {
			sha1_transform(ctx->state, &data[i]);
		}
		j = 0;
	} else
		i = 0;
	memcpy(&ctx->buf[j], &data[i], len - i);
}

static void sha1_finalize(struct sha1_ctx *ctx, sha1_digest *digest)
{
	int i;
	unsigned long long finalcount = be64_to_cpu(ctx->count << 3);

	sha1_update(ctx, (unsigned char *)"\200", 1);
	while ((ctx->count & 63) != 56)
		sha1_update(ctx, (unsigned char *) "", 1);

	sha1_update(ctx, (unsigned char *) &finalcount, 8);
	for (i = 0; i < 5; i++)
		digest->digest[i] = be32_to_cpu(ctx->state[i]);
}

static inline void sha1_to_hex(sha1_digest *digest, char *out)
{
	char *p;
	int i;
	for (p = out, i = 0; i < 20; i++, p += 2)
		snprintf(p, 3, "%02x", ((unsigned char *) digest->digest)[i]);
}

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>

#define GET_CTX_STRUCT(a) ((struct sha1_ctx *) a)

CAMLprim value stub_sha1_init(value unit)
{
	CAMLparam1(unit);
	CAMLlocal1(result);

        result = caml_alloc(sizeof(struct sha1_ctx), Abstract_tag);
	sha1_init(GET_CTX_STRUCT(result));

	CAMLreturn(result);
}

CAMLprim value stub_sha1_update(value ctx, value data, value ofs, value len)
{
	CAMLparam4(ctx, data, ofs, len);
	sha1_update(GET_CTX_STRUCT(ctx), (unsigned char *) data + Int_val(ofs),
	            Int_val(len));
	CAMLreturn(Val_unit);
}

CAMLprim value stub_sha1_finalize(value ctx)
{
	CAMLparam1(ctx);
	CAMLlocal1(t);

	t = caml_alloc(sizeof(sha1_digest), Abstract_tag);
	sha1_finalize(GET_CTX_STRUCT(ctx), (sha1_digest *) t);

	CAMLreturn(t);
}

CAMLprim value stub_sha1_to_hex(value t)
{
	CAMLparam1(t);
	CAMLlocal1(result);

	result = caml_alloc_string(40);
	sha1_to_hex((sha1_digest *) t, String_val(result));

	CAMLreturn(result);
}

/*
 * Local variables:
 *  indent-tabs-mode: t
 *  c-basic-offset: 8
 *  tab-width: 8
 * End:
 */
