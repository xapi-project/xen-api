/*
 * Copyright (c) Citrix Systems, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1) Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2) Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "xen_internal.h"
#include <xen/api/xen_common.h>
#include <xen/api/xen_event_batch.h>


static const struct_member xen_event_batch_struct_members [] = {
    { .key = "events",
        .type = &xen_event_record_set_abstract_type_,
        .offset = offsetof(xen_event_batch, events)},
    { .key = "valid_ref_counts",
        .type = &abstract_type_string_int_map,
        .offset = offsetof(xen_event_batch, valid_ref_counts)},
    { .key = "token",
        .type = &abstract_type_string,
        .offset = offsetof(xen_event_batch, token)},
};

const abstract_type xen_event_batch_abstract_type_ = {
    .typename = STRUCT,
    .struct_size = sizeof (xen_event_batch),
    .member_count =
    sizeof (xen_event_batch_struct_members) / sizeof (struct_member),
    .members = xen_event_batch_struct_members
};

void
xen_event_batch_free(xen_event_batch *batch)
{
    if (batch == NULL)
    {
        return;
    }
    xen_event_record_set_free(batch->events);
    xen_string_int_map_free(batch->valid_ref_counts);
    free(batch->token);
    free(batch);
}

bool
xen_event_from(xen_session *session, struct xen_event_batch **result, struct xen_string_set *classes, char *token, double timeout)
{
    abstract_value param_values[] = {
        { .type = &abstract_type_string_set,
            .u.set_val = (arbitrary_set *) classes},
        { .type = &abstract_type_string,
            .u.string_val = token},
        { .type = &abstract_type_float,
            .u.float_val = timeout}
    };
    abstract_type result_type = xen_event_batch_abstract_type_;
    *result = NULL;
    XEN_CALL_("event.from");
    return session->ok;
}
