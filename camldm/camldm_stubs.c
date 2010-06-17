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
#include <libdevmapper.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/* map is an array of 4-tuples 
 * (start : int64, size : int64, type : string, params : string)
 * format of params depends upon the type
 */
void camldm_create(value name, value map) 
{
  CAMLparam2(name,map);

  struct dm_task *dmt;
  int i;
  uint64_t start, size;
  char *ty,*params;

  if(!(dmt = dm_task_create(DM_DEVICE_CREATE)))
    caml_failwith("Failed to create task!");

  if(!dm_task_set_name(dmt, String_val(name))) {
    dm_task_destroy(dmt);
    caml_failwith("Failed to set name");
  }

  for(i=0; i<Wosize_val(map); i++) {
    start=Int64_val(Field(Field(map,i),0));
    size=Int64_val(Field(Field(map,i),1));
    ty=String_val(Field(Field(map,i),2));
    params=String_val(Field(Field(map,i),3));

    printf("%" PRIu64 " %" PRIu64 " %s %s\n", start, size, ty, params);

    if(!dm_task_add_target(dmt, start, size, ty, params)) {
      dm_task_destroy(dmt);
      caml_failwith("Failed to add target");
    }
  }
  
  if(!dm_task_run(dmt)) {
    dm_task_destroy(dmt);
    caml_failwith("Failed to run task");
  }
  
 win:
  CAMLreturn0;  
}

void camldm_reload(value name, value map) 
{
  CAMLparam2(name,map);

  struct dm_task *dmt;
  int i;
  uint64_t start, size;
  char *ty,*params;

  if(!(dmt = dm_task_create(DM_DEVICE_RELOAD)))
    caml_failwith("Failed to create task!");

  if(!dm_task_set_name(dmt, String_val(name))) 
    goto out;

  for(i=0; i<Wosize_val(map); i++) {
    start=Int64_val(Field(Field(map,i),0));
    size=Int64_val(Field(Field(map,i),1));
    ty=String_val(Field(Field(map,i),2));
    params=String_val(Field(Field(map,i),3));

    printf("%" PRIu64 " %" PRIu64 " %s %s\n", start, size, ty, params);

    if(!dm_task_add_target(dmt, start, size, ty, params))
      goto out;
  }
  
  if(!dm_task_run(dmt))
    goto out;

  goto win;

 out:
  dm_task_destroy(dmt);
  caml_failwith("Failed!");

 win:
  CAMLreturn0;  
}


void camldm_mknods(value dev)
{
  CAMLparam1 (dev);

  if(caml_string_length(dev)==0) {
    dm_mknodes(NULL);
  } else {
    dm_mknodes(String_val(dev));
  }
  
  CAMLreturn0;
}

value camldm_table(value dev)
{
  CAMLparam1 (dev);
  CAMLlocal4 (result,r,tuple,tmp);

  struct dm_task *dmt;
  struct dm_info info;

  void *next = NULL;
  uint64_t start, length;
  char *target_type = NULL;
  char *params = NULL;

  if(!(dmt = dm_task_create(DM_DEVICE_TABLE)))
    caml_failwith("Could not create dm_task");

  if(!dm_task_set_name(dmt, String_val(dev))) {
    dm_task_destroy(dmt);
    caml_failwith("Could not set device");
  }

  if(!dm_task_run(dmt)) {
    dm_task_destroy(dmt);
    caml_failwith("Failed to run task");
  }

  if (!dm_task_get_info(dmt, &info) || !info.exists) {
    dm_task_destroy(dmt);
    caml_failwith("Failed to get info");
  }

  result=caml_alloc_tuple(10);

  Store_field(result,0,Val_bool(info.exists));
  Store_field(result,1,Val_bool(info.suspended));
  Store_field(result,2,Val_bool(info.live_table));
  Store_field(result,3,Val_bool(info.inactive_table));
  Store_field(result,4,caml_copy_int32(info.open_count));
  Store_field(result,5,caml_copy_int32(info.event_nr));
  Store_field(result,6,caml_copy_int32(info.major));
  Store_field(result,7,caml_copy_int32(info.minor));
  Store_field(result,8,Val_bool(info.read_only));

  tmp=Val_int(0);

  do {
    next = dm_get_next_target(dmt, next, &start, &length, &target_type, &params);

    /* This is how dmsetup.c checks for an empty table: */
    if (!target_type)
      continue;

    dm_task_get_info(dmt, &info);

    tuple=caml_alloc_tuple(4);
    Store_field(tuple,0,caml_copy_int64(start));
    Store_field(tuple,1,caml_copy_int64(length));
    Store_field(tuple,2,caml_copy_string(target_type));
    Store_field(tuple,3,caml_copy_string(params));

    r=caml_alloc(2,0);
    Store_field(r, 0, tuple);
    Store_field(r, 1, tmp);

    tmp=r;
  } while(next);

  Store_field(result,9,tmp);

  CAMLreturn(result);
}

void _simple(int task, const char *name) 
{
  struct dm_task *dmt;

  if (!(dmt = dm_task_create(task)))
    caml_failwith("Failed to create task");

  if(!dm_task_set_name(dmt, name)) {
    dm_task_destroy(dmt);
    caml_failwith("Could not set device");
  }
  
  if(!dm_task_run(dmt)) {
    dm_task_destroy(dmt);
    caml_failwith("Failed to run task");
  }

  dm_task_destroy(dmt);
}

void camldm_remove(value device)
{
  CAMLparam1(device);
  _simple(DM_DEVICE_REMOVE,String_val(device));
  CAMLreturn0;
}

void camldm_suspend(value device)
{
  CAMLparam1(device);
  _simple(DM_DEVICE_SUSPEND,String_val(device));
  CAMLreturn0;
}

void camldm_resume(value device)
{
  CAMLparam1(device);
  _simple(DM_DEVICE_RESUME,String_val(device));
  CAMLreturn0;
}

void camldm_mknod(value path, value mode, value major, value minor)
{
  CAMLparam4(path, mode, major, minor);
  mknod(String_val(path),S_IFBLK | Int_val(mode), makedev(Int_val(major),Int_val(minor)));
  CAMLreturn0;
}


/* Helper functions for camldm_ls */

#define none Val_int(0)
#define Tag_some Val_int(0)

value some (value content) {
  CAMLparam1 (content);
  CAMLlocal1 (result);
  result = caml_alloc (1, Tag_some);
  Store_field (result, 0, content);
  CAMLreturn (result);
};
value cons (value car_value, value cdr_value) {
  CAMLparam2 (car_value, cdr_value);
  CAMLlocal1 (cell);
  
  const int car = 0;
  const int cdr = 1;
  cell = caml_alloc (2, Tag_cons);
  Store_field (cell, car, car_value);
  Store_field (cell, cdr, cdr_value);
  
  CAMLreturn (cell);
};
/*
  camldm_ls may leak memory.  Who knows?  (Does the c function (_process_all)
  where I copied this from (dmsetup.c) care about memory?  dmsetup
  exits shortly after executing it, anyway.
  
  After testing: It does _not_ seem to leak.  Probably
  "dm_task_destroy(dmt);" is doing some cleaning up.
*/
value camldm_ls()
{
  CAMLparam0 ();
  CAMLlocal1 (list);
  
  struct dm_names *names;
  struct dm_task *dmt;

  if (!(dmt = dm_task_create(DM_DEVICE_LIST)))
    CAMLreturn(none);
  
  if (!dm_task_run(dmt)) {
    dm_task_destroy(dmt);
    CAMLreturn(none);
  }
  
  if (!(names = dm_task_get_names(dmt))) {
    dm_task_destroy(dmt);
    CAMLreturn(none);
  }
  
  list = Val_emptylist;
  if (!names->dev) {
    dm_task_destroy(dmt);
    CAMLreturn(some(list));
  }

  unsigned int next = 0;

  do {
    names = (void *) names + next;
    //    printf("%s\t(%d, %d)\n", names->name,
    //	   (int) MAJOR(names->dev), (int) MINOR(names->dev));
    
    list = cons (caml_copy_string(names->name), list);
    
    // printf("%s\t(:Debug only)\n", names->name);
    next = names->next;
  } while (next);

  dm_task_destroy(dmt);
  CAMLreturn(some(list));
}
