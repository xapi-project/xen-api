/*
 * Copyright (c) 2015 Citrix Inc
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
 */

//Provides: caml_alloc_pages
//Requires: caml_ba_create_from
//Requires: caml_ba_views
//Requires: caml_ba_init_views
//Requires: caml_ba_get_size
function caml_alloc_pages(npages) {
  caml_ba_init_views();
  var dims = [ npages * 4096 ];
  var size = caml_ba_get_size(dims);
  var kind = 0;
  var layout = 0;
  var view = caml_ba_views[0][kind];
  var data = new view(size);
  var data_type = caml_ba_views[1][kind];
  var data2 = null;
  return caml_ba_create_from(data, data2, data_type, kind, layout, dims);
}
