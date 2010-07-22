#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <vhd/libvhd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>

#include "syslog.h"


static struct custom_operations vhd_ops = {
  "com.citrix.dci.vhd",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define Vhd_val(v) (*((vhd_context_t **) Data_custom_val(v)))

static value alloc_vhd(vhd_context_t *t)
{
  value v = alloc_custom(&vhd_ops, sizeof(vhd_context_t *), 0, 1);
  Vhd_val(v)=t;
  return v;
}

value stub_vhd_open(value name, value flags) 
{
  CAMLparam2(name,flags);
  CAMLlocal1(vhd);
  vhd_context_t *context = (vhd_context_t *)malloc(sizeof(vhd_context_t));
  int ret = vhd_open(context,String_val(name),Int_val(flags));
  if(ret!=0) {
    caml_failwith("Failed to open VHD");
  }
  vhd=alloc_vhd(context);
  CAMLreturn (vhd);
}

value stub_vhd_close(value vhd)
{
  CAMLparam1(vhd);
  vhd_context_t *context=Vhd_val(vhd);
  vhd_close(context);
  free(context);
  CAMLreturn (Val_unit);
}
  
value stub_vhd_get_phys_size(value vhd)
{
  CAMLparam1(vhd);
  vhd_context_t *context=Vhd_val(vhd);
  off64_t phys_size;
  vhd_get_phys_size(context, &phys_size);
  CAMLreturn(caml_copy_int64(phys_size));
}

value stub_vhd_create(value name, value size, value type, value mbytes, value flags)
{
  CAMLparam5(name,size,type,mbytes,flags);
  int ret = vhd_create(String_val(name),Int64_val(size),Int_val(type),Int64_val(mbytes),Int_val(flags));
  if(ret != 0) {
    caml_failwith("Failed to create VHD");
  }
  CAMLreturn (Val_unit);
}

value stub_vhd_snapshot(value snapshot, value size, value parent, value mbytes, value flags)
{
  CAMLparam5(snapshot,size,parent,mbytes,flags);
  int ret = vhd_snapshot(String_val(snapshot),Int64_val(size),String_val(parent),Int64_val(mbytes),Int_val(flags));
  if(ret != 0) {
    caml_failwith("Failed to snapshot VHD");
  }
  CAMLreturn (Val_unit);
}

value stub_vhd_get_parent(value vhd)
{
  CAMLparam1(vhd);
  char *parent=NULL;
  int n,i,err;
  vhd_parent_locator_t *loc;

  vhd_context_t *context = Vhd_val(vhd);

  if(context->footer.type != HD_TYPE_DIFF) {
    caml_failwith("Disk is not a differencing disk");
  }

  n = vhd_parent_locator_count(context);
  for (i = 0; i < n; i++) {
	loc = context->header.loc + i;
	if(loc->code == PLAT_CODE_MACX) {
	  err = vhd_parent_locator_read(context, loc, &parent);
	  if (err)
		caml_failwith("vhd_parent_locator_read failed");
	}
  }

  if(parent==NULL) {
	caml_failwith("Failed to find a parent!");
  }

  CAMLreturn(caml_copy_string(parent));
}

value stub_vhd_get_uid(value vhd)
{
  CAMLparam1(vhd);
  vhd_context_t *context = Vhd_val(vhd);
  char uuid[256];
  uuid_unparse_lower(context->footer.uuid,uuid);
  CAMLreturn(caml_copy_string(uuid));
}

value stub_vhd_get_max_bat_size(value vhd)
{
  CAMLparam1(vhd);
  vhd_context_t *context = Vhd_val(vhd);
  CAMLreturn(caml_copy_int64(context->header.max_bat_size));
}

value stub_vhd_get_parent_uid(value vhd)
{
  CAMLparam1(vhd);
  vhd_context_t *context = Vhd_val(vhd);
  char uuid[256];
  if(context->footer.type != HD_TYPE_DIFF) {
    caml_failwith("Not a differencing disk");
  }
  uuid_unparse_lower(context->header.prt_uuid,uuid);
  CAMLreturn(caml_copy_string(uuid));
}

value stub_vhd_get_type(value vhd)
{
  CAMLparam1(vhd);
  vhd_context_t *context = Vhd_val(vhd);
  CAMLreturn(Val_int(context->footer.type));
}

value stub_vhd_get_creator(value vhd)
{
  CAMLparam1(vhd);
  vhd_context_t *context = Vhd_val(vhd);
  char creator[5];
  strncpy(creator,context->footer.crtr_app,4);
  creator[4]='\0';
  CAMLreturn(caml_copy_string(creator));
}

value stub_vhd_get_virtual_size(value vhd)
{
  CAMLparam1(vhd);
  vhd_context_t *context = Vhd_val(vhd);
  CAMLreturn(caml_copy_int64(context->footer.curr_size));
}

value stub_vhd_get_hidden(value vhd)
{
  CAMLparam1(vhd);
  int hidden;
  vhd_context_t *context = Vhd_val(vhd);
  vhd_hidden(context,&hidden);
  CAMLreturn(Val_int(hidden));
}

value stub_vhd_set_hidden(value vhd, value hidden)
{
  CAMLparam2(vhd,hidden);
  vhd_context_t *context = Vhd_val(vhd);
  context->footer.hidden = (char)Int_val(hidden);
  int err = vhd_write_footer(context, &context->footer);
  if(err) {
    syslog(LOG_DAEMON | LOG_ERR, "set hidden failed: %d", err);
    caml_failwith("Set hidden failed!");
  }
  CAMLreturn(Val_unit);
}

value stub_vhd_set_phys_size(value vhd, value ml_newsize)
{
  CAMLparam2(vhd,ml_newsize);
  int err;
  vhd_context_t *context = Vhd_val(vhd);
  uint64_t newsize=Int64_val(ml_newsize);
  err=vhd_set_phys_size(context, newsize);
  if(err) {
    syslog(LOG_DAEMON | LOG_ERR, "vhd_set_phys_size failed: %d", err);
    caml_failwith("Set phys_size failed");
  }
  CAMLreturn(Val_unit);
}

value stub_vhd_set_virt_size(value vhd, value ml_newsize)
{
  CAMLparam2(vhd,ml_newsize);
  int err;
  vhd_context_t *context = Vhd_val(vhd);
  uint64_t newsize=Int64_val(ml_newsize);
  err=vhd_set_virt_size(context, newsize);
  if(err) {
    syslog(LOG_DAEMON | LOG_ERR, "vhd_set_virt_size failed: %d", err);
    caml_failwith("Set virt_size failed");
  }
  CAMLreturn(Val_unit);
}

static int
__raw_io_write(int fd, char* buf, uint64_t sec, uint32_t secs)
{
        off64_t off;
        size_t ret;

        errno = 0;
        off = lseek64(fd, vhd_sectors_to_bytes(sec), SEEK_SET);
        if (off == (off64_t)-1) {
                printf("raw parent: seek(0x%08"PRIx64") failed: %d\n",
                       vhd_sectors_to_bytes(sec), -errno);
                return -errno;
        }

        ret = write(fd, buf, vhd_sectors_to_bytes(secs));
        if (ret == vhd_sectors_to_bytes(secs))
                return 0;

        printf("raw parent: write of 0x%"PRIx64" returned %zd, errno: %d\n",
               vhd_sectors_to_bytes(secs), ret, -errno);
        return (errno ? -errno : -EIO);
}

/*
 * Use 'parent' if the parent is VHD, and 'parent_fd' if the parent is raw
 */
static int
vhd_util_coalesce_block(vhd_context_t *vhd, vhd_context_t *parent,
                int parent_fd, uint64_t block)
{
        int i, err;
        char *buf, *map;
        uint64_t sec, secs;

        buf = NULL;
        map = NULL;
        sec = block * vhd->spb;

        if (vhd->bat.bat[block] == DD_BLK_UNUSED)
                return 0;

        err = posix_memalign((void **)&buf, 4096, vhd->header.block_size);
        if (err)
                return -err;

        err = vhd_io_read(vhd, buf, sec, vhd->spb);
        if (err)
                goto done;

        if (vhd_has_batmap(vhd) && vhd_batmap_test(vhd, &vhd->batmap, block)) {
                if (parent->file)
                        err = vhd_io_write(parent, buf, sec, vhd->spb);
                else
                        err = __raw_io_write(parent_fd, buf, sec, vhd->spb);
                goto done;
        }

        err = vhd_read_bitmap(vhd, block, &map);
        if (err)
                goto done;

        for (i = 0; i < vhd->spb; i++) {
                if (!vhd_bitmap_test(vhd, map, i))
                        continue;

                for (secs = 0; i + secs < vhd->spb; secs++)
                        if (!vhd_bitmap_test(vhd, map, i + secs))
                                break;

                if (parent->file)
                        err = vhd_io_write(parent,
                                           buf + vhd_sectors_to_bytes(i),
                                           sec + i, secs);
                else
                        err = __raw_io_write(parent_fd,
                                             buf + vhd_sectors_to_bytes(i),
                                             sec + i, secs);
                if (err)
                        goto done;

                i += secs;
        }

        err = 0;

done:
        free(buf);
        free(map);
        return err;
}

value stub_vhd_coalesce(value vhd) 
{
  CAMLparam1(vhd);
  vhd_context_t *context = Vhd_val(vhd);
  vhd_context_t parent;
  char uuid[37];
  char *pname;
  int err,i;
  int parent_fd=-1;

  parent.file = NULL;

  uuid_unparse(context->footer.uuid,uuid);

  caml_enter_blocking_section();

  err=vhd_parent_locator_get(context, &pname);
  if(err) {
    syslog(LOG_DAEMON | LOG_ERR, "Error finding %s parent: %d", uuid, err);
    caml_failwith("Couldn't find parent");
  }

  if( vhd_parent_raw(context)) {
    parent_fd = open(pname, O_RDWR | O_DIRECT | O_LARGEFILE, 0644);
    if(parent_fd == -1) {
      err = - errno;
      syslog(LOG_DAEMON | LOG_ERR, "Failed to open raw parent %s: %d",pname,err);
      free(pname);
      caml_failwith("Couldn't open parent");
    }
  } else {
    err = vhd_open(&parent,pname,VHD_OPEN_RDWR);
    if(err) {
      syslog(LOG_DAEMON | LOG_ERR, "Failed to open vhd parent %s: %d",pname,err);
      free(pname);
      caml_failwith("Couldn't open parent");
    }
  }

  err=vhd_get_bat(context);
  if(err)
    goto done;

  if(vhd_has_batmap(context)) {
    err = vhd_get_batmap(context);
    if (err)
      goto done;
  }

  for(i=0; i<context->bat.entries; i++) {
    err=vhd_util_coalesce_block(context,&parent,parent_fd,i);
    if(err)
      goto done;
  }

  err=0;

 done:
  free(pname);
  if(parent.file)
    vhd_close(&parent);
  else
    close(parent_fd);

  caml_leave_blocking_section();
  
  CAMLreturn (Val_unit);
}

value stub_vhd_write_sector(value vhd, value ml_sectorno, value ml_string)
{
  CAMLparam3(vhd, ml_sectorno, ml_string);
  uint64_t sectorno=Int64_val(ml_sectorno);
  if(caml_string_length(ml_string)!=512) 
    caml_failwith("Require string to be of length 512");
  vhd_context_t *context = Vhd_val(vhd);
  char *buf;
  int err;

  err = posix_memalign((void **)&buf, 4096, context->header.block_size);

  if(err) {
    syslog(LOG_INFO, "error with the posix_memalign: %d", err);
    caml_failwith("Error with the posix memalign");
  }

  memcpy(buf,String_val(ml_string),512);

  caml_enter_blocking_section();

  err = vhd_get_bat(context);

  if(err) {
    syslog(LOG_INFO, "error getting bat: %d", err);
    caml_leave_blocking_section();
    caml_failwith("Error getting BAT");
  }

  err = vhd_io_write(context, buf, sectorno, 1);

  if(err) {
    syslog(LOG_INFO, "error performing write: %d", err);
    caml_leave_blocking_section();
    caml_failwith("Error performing write");
  }

  syslog(LOG_INFO, "string='%s', sectorno=%Ld, err=%d", buf, sectorno, err);

  caml_leave_blocking_section();

  CAMLreturn(Val_int(err));
}

value stub_vhd_read_sector(value vhd, value ml_sectorno)
{
  CAMLparam2(vhd,ml_sectorno);
  CAMLlocal1(returnstr);
  char buf[512];
  uint64_t sectorno=Int64_val(ml_sectorno);
  vhd_context_t *context = Vhd_val(vhd);
  int err;
  
  caml_enter_blocking_section();
  err = vhd_io_read(context, buf, sectorno, 1);
  caml_leave_blocking_section();

  returnstr=caml_alloc_string(512);
  memcpy(String_val(returnstr),buf,512);
  CAMLreturn(returnstr);  
}

value stub_vhd_set_log_level(value level)
{
  CAMLparam1(level);
  libvhd_set_log_level(Int_val(level));
  CAMLreturn(Val_unit);
}

value stub_vhd_set_parent(value vhd, value ml_new_parent, value ml_new_parent_is_raw)
{
  CAMLparam3(vhd, ml_new_parent, ml_new_parent_is_raw);
  char *new_parent = strdup(String_val(ml_new_parent));
  int new_parent_is_raw = 0;
  vhd_context_t *context = Vhd_val(vhd);
  int err;

  if(Bool_val(ml_new_parent_is_raw))
	new_parent_is_raw=1;

  caml_enter_blocking_section();
  err=vhd_change_parent(context, new_parent, new_parent_is_raw);
  if(err) {
    syslog(LOG_INFO, "error performing setting parent: %d", err);
  }
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

/* Return a run-length encoded list of allocated blocks */
value stub_vhd_get_bat(value vhd)
{
  CAMLparam1(vhd);
  CAMLlocal3(list,tmp,pair);
  vhd_context_t *context = Vhd_val(vhd);
  int state=0;
  int len=0;
  int i;
  int max = context->footer.curr_size >> 21;

  int err = vhd_get_bat(context);

  syslog(LOG_DAEMON | LOG_ERR, "stub_vhd_get_bat: max=%d",max);

  if(err != 0) {
    caml_failwith("Failed to get BAT");
  }

  list = Val_int(0);

  for(i=0; i<max; i++) {
    if(state==0) {
      if(context->bat.bat[i] != DD_BLK_UNUSED) {
        state=1;
        pair = caml_alloc(2,0);
        Store_field(pair,0,Val_int(i));
        len=1;            
      }
    } else if(state==1) {
      if(context->bat.bat[i] == DD_BLK_UNUSED) {
        Store_field(pair,1,Val_int(len));
        tmp = caml_alloc(2,0);
        Store_field(tmp,0,pair);
        Store_field(tmp,1,list);
        list=tmp;
        state=0;
        len=0;
      } else {
        len++;
      }
    }
  }

  if(state==1) {
    Store_field(pair,1,Val_int(len));
    tmp = caml_alloc(2,0);
    Store_field(tmp,0,pair);
    Store_field(tmp,1,list);
    list=tmp;
  }


  CAMLreturn(list);
}

value stub_vhd_get_first_allocated_block(value vhd)
{
  CAMLparam1(vhd);
  vhd_context_t *context = Vhd_val(vhd);
  uint64_t firstblock=DD_BLK_UNUSED;
  int i,max,err;

  max = context->footer.curr_size >> 21;
  err = vhd_get_bat(context);
  
  for(i=0; i<max; i++) {
    if(context->bat.bat[i]<firstblock) {
      firstblock=context->bat.bat[i];
    }
  }  

  CAMLreturn(caml_copy_int64(firstblock));
}
