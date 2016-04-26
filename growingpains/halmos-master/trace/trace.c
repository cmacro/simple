/* */
/* use gcc features for profiling function calls */
/* usage */
/* compile the code to instrument with -c and -finstrument-functions */
/* compile this file with gcc -c FILENAME -o FILENAME.o, without the */
/* -finstrument-functions (otherwise it will crash) */
/* link them together */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

// #include <mach/mach.h>
// #include <mach/mach_vm.h>
// #include <mach-o/getsect.h>

static FILE* trace;

static const char* trace_file = "trace.out";
void
__attribute__((constructor))
begin_trace(void)
{
  trace = fopen(trace_file, "w");
  if (trace == NULL) {
    fprintf(stderr, "failed to open %s\n", trace_file);
    exit(0);
  }
  // printf("etext %p\n", (void*)get_etext());
/* get the base address of the process */

/* old version: this is most probably wrong */
  // pid_t pid = getpid();
  // mach_port_t task;
  // kern_return_t kern_return = task_for_pid(mach_task_self(), pid, &task);
  // if (kern_return != KERN_SUCCESS) {
  //   printf("task_for_pid failed, error [%s]", mach_error_string(kern_return));
  //   exit(1);
  // }
  // kern_return_t kret;
  // vm_region_basic_info_data_t info;
  // mach_vm_size_t size;
  // mach_port_t object_name;
  // mach_msg_type_number_t count;
  // mach_vm_address_t address = 1;
  // kret = mach_vm_region(task, &address, &size, VM_REGION_BASIC_INFO,
  //   (vm_region_info_t) &info, &count, &object_name);
  // if (kret != KERN_SUCCESS) {
  //   printf("mach_vm_region failed, error [%s]",
  //    mach_error_string(kern_return));
  //   exit(1);
  // }
  // printf("base addr: %llx\n", address);
}

void
__attribute__((destructor))
end_trace(void)
{
  if (trace != NULL) {
    fclose(trace);
    printf("wrote trace log to %s\n", trace_file);
  }
}
#include <stdlib.h>
void
__cyg_profile_func_enter(void* this_fn, void* call_site)
{
  if (trace != NULL) {
    fprintf(trace, "e %p %p %lu\n", this_fn, call_site, clock());
  }
}

void
__cyg_profile_func_exit(void* this_fn, void* call_site)
{
  if (trace != NULL) {
    fprintf(trace, "x %p %p %lu\n", this_fn, call_site, clock());
  }
}
