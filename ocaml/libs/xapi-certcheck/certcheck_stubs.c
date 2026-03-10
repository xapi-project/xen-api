/*
 * OCaml C stubs for libvarstored-certcheck.
 *
 * Bridges the C certcheck API to OCaml values.
 */

#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include "certcheck.h"

/* Map C status codes to OCaml variant tag integers (must match certcheck.ml). */
static value
status_to_ocaml(cert_check_status_t s)
{
    switch (s) {
    case CERT_CHECK_OK:             return Val_int(0);
    case CERT_CHECK_EXPIRED:        return Val_int(1);
    case CERT_CHECK_NOT_YET_VALID:  return Val_int(2);
    case CERT_CHECK_PARSE_ERROR:    return Val_int(3);
    case CERT_CHECK_NO_CERTS:       return Val_int(4);
    case CERT_CHECK_INTERNAL_ERROR: return Val_int(5);
    default:                        return Val_int(5);
    }
}

/* val check_expiration : bytes -> status */
CAMLprim value
caml_varstored_check_cert_expiration(value v_data)
{
    CAMLparam1(v_data);
    const uint8_t *data = (const uint8_t *)Bytes_val(v_data);
    size_t len = caml_string_length(v_data);
    CAMLreturn(status_to_ocaml(varstored_check_cert_expiration(data, len)));
}

/* val check_expiration_at : bytes -> int64 -> status */
CAMLprim value
caml_varstored_check_cert_expiration_at(value v_data, value v_time)
{
    CAMLparam2(v_data, v_time);
    const uint8_t *data = (const uint8_t *)Bytes_val(v_data);
    size_t len = caml_string_length(v_data);
    time_t t = (time_t)Int64_val(v_time);
    CAMLreturn(status_to_ocaml(
        varstored_check_cert_expiration_at(data, len, t)));
}

/*
 * Convert one cert_info_t node to an OCaml record.
 *
 * Record field order (must match certcheck.ml type cert_info):
 *   0: subject        string
 *   1: issuer         string
 *   2: not_before     int64
 *   3: not_after      int64
 *   4: is_expired     bool
 *   5: is_not_yet_valid bool
 *   6: variable_name  string
 */
static value
cert_info_to_ocaml(const cert_info_t *info)
{
    CAMLparam0();
    CAMLlocal1(rec);

    rec = caml_alloc(7, 0);
    Store_field(rec, 0, caml_copy_string(info->subject       ? info->subject       : ""));
    Store_field(rec, 1, caml_copy_string(info->issuer        ? info->issuer        : ""));
    Store_field(rec, 2, caml_copy_int64((int64_t)info->not_before));
    Store_field(rec, 3, caml_copy_int64((int64_t)info->not_after));
    Store_field(rec, 4, Val_bool(info->is_expired));
    Store_field(rec, 5, Val_bool(info->is_not_yet_valid));
    Store_field(rec, 6, caml_copy_string(info->variable_name ? info->variable_name : ""));

    CAMLreturn(rec);
}

/* val get_cert_info : bytes -> status * cert_info list */
CAMLprim value
caml_varstored_get_cert_info(value v_data)
{
    CAMLparam1(v_data);
    CAMLlocal4(result, cert_list, cons, info_val);

    const uint8_t *data = (const uint8_t *)Bytes_val(v_data);
    size_t len = caml_string_length(v_data);
    cert_info_t *certs = NULL;
    const cert_info_t *cur;

    cert_check_status_t status = varstored_get_cert_info(data, len, &certs);

    /* Build OCaml list in forward order by traversing C list twice:
       first pass counts / second pass builds in reverse then we reverse.
       Simpler: collect into a temporary reversed list, then reverse it. */
    cert_list = Val_emptylist;
    for (cur = certs; cur; cur = cur->next) {
        info_val = cert_info_to_ocaml(cur);
        cons = caml_alloc(2, 0);
        Store_field(cons, 0, info_val);
        Store_field(cons, 1, cert_list);
        cert_list = cons;
    }
    varstored_free_cert_info(certs);

    /* Reverse the list so that it matches the C traversal order. */
    {
        CAMLlocal2(rev, tail);
        rev = Val_emptylist;
        while (cert_list != Val_emptylist) {
            tail = Field(cert_list, 1);
            Store_field(cert_list, 1, rev);
            rev = cert_list;
            cert_list = tail;
        }
        cert_list = rev;
    }

    result = caml_alloc(2, 0);
    Store_field(result, 0, status_to_ocaml(status));
    Store_field(result, 1, cert_list);

    CAMLreturn(result);
}

/* val status_string : status -> string */
CAMLprim value
caml_varstored_cert_status_string(value v_status)
{
    CAMLparam1(v_status);
    cert_check_status_t status;

    switch (Int_val(v_status)) {
    case 0:  status = CERT_CHECK_OK;             break;
    case 1:  status = CERT_CHECK_EXPIRED;        break;
    case 2:  status = CERT_CHECK_NOT_YET_VALID;  break;
    case 3:  status = CERT_CHECK_PARSE_ERROR;    break;
    case 4:  status = CERT_CHECK_NO_CERTS;       break;
    default: status = CERT_CHECK_INTERNAL_ERROR; break;
    }

    CAMLreturn(caml_copy_string(varstored_cert_status_string(status)));
}
