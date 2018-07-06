## 1.1.0 (2017-05-22)

- `resize` should be able to return `write_error`
- update `appveyor.yml`
- build with `jbuilder`

## 1.0.0 (2016-12-21)

- Import `V1.BLOCK` from `mirage-types` into `Mirage_block.S` (@samoht)
- Import `V1_LWT.BLOCK` from `mirage-types-lwt` into `Mirage_bloc_lwt.S` (@samoht)

### 0.2 (2015-11-09)

- add `Error.string_of_error`
- clarify that `fold_mapped` callbacks use sectors, not bytes
- bugfix `fold_mapped_s`
- `fold_unmapped_s` should not return the empty buffers: lengths are enough

### 0.1 (2015-11-03)

- initial version
