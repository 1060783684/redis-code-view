/* zmalloc - total amount of allocated memory aware version of malloc()
 *
 * Copyright (c) 2009-2010, Salvatore Sanfilippo <antirez at gmail dot com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Redis nor the names of its contributors may be used
 *     to endorse or promote products derived from this software without
 *     specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>

/* This function provide us access to the original libc free(). This is useful
 * for instance to free results obtained by backtrace_symbols(). We need
 * to define this function before including zmalloc.h that may shadow the
 * free implementation if we use jemalloc or another non standard allocator. */
void zlibc_free(void *ptr) {
    free(ptr);
}

#include <string.h>
#include <pthread.h>
#include "config.h"
#include "zmalloc.h"

#ifdef HAVE_MALLOC_SIZE
#define PREFIX_SIZE (0)
#else
#if defined(__sun) || defined(__sparc) || defined(__sparc__)
#define PREFIX_SIZE (sizeof(long long))
#else
#define PREFIX_SIZE (sizeof(size_t))
#endif
#endif

/* Explicitly override malloc/free etc when using tcmalloc. */
#if defined(USE_TCMALLOC)
#define malloc(size) tc_malloc(size)
#define calloc(count,size) tc_calloc(count,size)
#define realloc(ptr,size) tc_realloc(ptr,size)
#define free(ptr) tc_free(ptr)
#elif defined(USE_JEMALLOC)
#define malloc(size) je_malloc(size)
#define calloc(count,size) je_calloc(count,size)
#define realloc(ptr,size) je_realloc(ptr,size)
#define free(ptr) je_free(ptr)
#endif

#if defined(__ATOMIC_RELAXED)
#define update_zmalloc_stat_add(__n) __atomic_add_fetch(&used_memory, (__n), __ATOMIC_RELAXED)
#define update_zmalloc_stat_sub(__n) __atomic_sub_fetch(&used_memory, (__n), __ATOMIC_RELAXED)
#elif defined(HAVE_ATOMIC)
#define update_zmalloc_stat_add(__n) __sync_add_and_fetch(&used_memory, (__n))
#define update_zmalloc_stat_sub(__n) __sync_sub_and_fetch(&used_memory, (__n))
#else
#define update_zmalloc_stat_add(__n) do { \
    pthread_mutex_lock(&used_memory_mutex); \
    used_memory += (__n); \
    pthread_mutex_unlock(&used_memory_mutex); \
} while(0)

#define update_zmalloc_stat_sub(__n) do { \
    pthread_mutex_lock(&used_memory_mutex); \
    used_memory -= (__n); \
    pthread_mutex_unlock(&used_memory_mutex); \
} while(0)

#endif

#define update_zmalloc_stat_alloc(__n) do { \
    size_t _n = (__n); \
// 若n不到一个long的长度,则内存对齐至long的整数倍长度,这么做是因为,当使用malloc开辟一块空间时,若申请的长度不够一个long的长度,则会默认申请一个
// long长度的空间.而若申请的空间不够long的整数倍,则会默认申请大于我们想要的空间的大小的long的整数倍空间
    if (_n&(sizeof(long)-1)) _n += sizeof(long)-(_n&(sizeof(long)-1)); \
    if (zmalloc_thread_safe) { \
        update_zmalloc_stat_add(_n); \
    } else { \
        used_memory += _n; \
    } \
} while(0)

// 使用do while应该是想将为完成操作而声明的局部变量的作用范围限制在语句块儿内，不影响其他函数的变量声明
#define update_zmalloc_stat_free(__n) do { \
    size_t _n = (__n); \
    if (_n&(sizeof(long)-1)) _n += sizeof(long)-(_n&(sizeof(long)-1)); \
    if (zmalloc_thread_safe) { \
        update_zmalloc_stat_sub(_n); \
    } else { \
        used_memory -= _n; \
    } \
} while(0)

static size_t used_memory = 0; // 全局静态变量，保存总共申请了多少字节的内存
static int zmalloc_thread_safe = 0; // zmalloc是否是线程安全的
pthread_mutex_t used_memory_mutex = PTHREAD_MUTEX_INITIALIZER; // 当没有cas函数时，修改used_memory变量时用于同步用的互斥量
                                                                // 若有cas函数则使用cas函数修改used_memory变量

// 默认zmalloc oom处理方法，redis.c中有其他的方法将其覆盖
static void zmalloc_default_oom(size_t size) {
    fprintf(stderr, "zmalloc: Out of memory trying to allocate %zu bytes\n",
        size); // 将size的内容替换掉%zu后将替换后的字符串通过stderr流输出
    fflush(stderr); // 刷新stderr流的缓冲区
    abort(); // 终止一个进程
}

static void (*zmalloc_oom_handler)(size_t) = zmalloc_default_oom;

void *zmalloc(size_t size) {
    void *ptr = malloc(size+ PREFIX_SIZE );
    
    if (!ptr) zmalloc_oom_handler(size);
// 有这个宏说明所使用的malloc库提供了获取申请空间大小的api,不需要额外的存储空间记录当前申请的内存空间的大小
#ifdef HAVE_MALLOC_SIZE
    update_zmalloc_stat_alloc(zmalloc_size(ptr));
    return ptr;
// 没有 HAVE_MALLOC_SIZE 这个宏说明malloc库不提供获取当前内存大小的api,需要额外申请一个PREFIX_SIZE大小的空间记录当前申请的空间的大小
#else
    *((size_t*)ptr) = size;
    update_zmalloc_stat_alloc(size+PREFIX_SIZE);
    return (char*)ptr+PREFIX_SIZE;
#endif
}

void *zcalloc(size_t size) {
    void *ptr = calloc(1, size+PREFIX_SIZE);

    if (!ptr) zmalloc_oom_handler(size);
#ifdef HAVE_MALLOC_SIZE
    update_zmalloc_stat_alloc(zmalloc_size(ptr));
    return ptr;
#else
    *((size_t*)ptr) = size;
    update_zmalloc_stat_alloc(size+PREFIX_SIZE);
    return (char*)ptr+PREFIX_SIZE;
#endif
}

void *zrealloc(void *ptr, size_t size) {
#ifndef HAVE_MALLOC_SIZE
    void *realptr; // 当HAVE_MALLOC_SIZE宏不存在时,需要这个临时变量存储PREFIX_SIZE的首地址,即整个空间首地址
#endif
    size_t oldsize;
    void *newptr;

    if (ptr == NULL) return zmalloc(size);
#ifdef HAVE_MALLOC_SIZE
    // 若存在HAVE_MALLOC_SIZE宏,则直接通过api获取内存大小,也不需要PREFIX_SIZE空间了,减少了对PREFIX_SIZE空间的操作步骤
    oldsize = zmalloc_size(ptr);
    newptr = realloc(ptr,size);
    if (!newptr) zmalloc_oom_handler(size);

    // 减去老内存的大小
    update_zmalloc_stat_free(oldsize);
    // 增加新内存的大小
    update_zmalloc_stat_alloc(zmalloc_size(newptr));
    return newptr;
// 若未声明HAVE_MALLOC_SIZE宏,则需要对PREFIX_SIZE空间做额外的操作
#else
    // ptr指针为了使用方便的缘故一般都在PREFIX_SIZE空间后,即真正使用的空间的首部,所以若想要获取PREFIX_SIZE内存放的数据(申请的空间的大小)
    // 则需要将指针移动到PREFIX_SIZE空间的首部
    realptr = (char*)ptr-PREFIX_SIZE;
    
    // 获取老的内存空间的大小
    oldsize = *((size_t*)realptr);
    
    // 重新申请新的内存
    newptr = realloc(realptr,size+PREFIX_SIZE);
    
    // 若没申请下来则,则调用out of memory处理函数,基本上就直接退出进程了
    if (!newptr) zmalloc_oom_handler(size);

    // 设置新空间的size到PREFIX_SIZE空间中
    *((size_t*)newptr) = size;
    // 由于realloc方法内部会对旧的空间进行释放处理,其实也有可能只是在原有地址的基础上扩充了一部分,所以老地址的释放不用我们操心
    // 减去老内存的大小
    update_zmalloc_stat_free(oldsize);
    // 增加新内存的大小
    update_zmalloc_stat_alloc(size);
    // 返回真实使用空间的首地址,即PREFIX_SIZE空间的尾地址的后一个地址
    return (char*)newptr+PREFIX_SIZE;
#endif
}

/* Provide zmalloc_size() for systems where this function is not provided by
 * malloc itself, given that in that case we store a header with this
 * information as the first bytes of every allocation. */
#ifndef HAVE_MALLOC_SIZE
size_t zmalloc_size(void *ptr) {
    void *realptr = (char*)ptr-PREFIX_SIZE;
    size_t size = *((size_t*)realptr);
    /* Assume at least that all the allocations are padded at sizeof(long) by
     * the underlying allocator. */
    if (size&(sizeof(long)-1)) size += sizeof(long)-(size&(sizeof(long)-1));
    return size+PREFIX_SIZE;
}
#endif

void zfree(void *ptr) {
#ifndef HAVE_MALLOC_SIZE
    void *realptr;
    size_t oldsize;
#endif

    if (ptr == NULL) return;
#ifdef HAVE_MALLOC_SIZE
    update_zmalloc_stat_free(zmalloc_size(ptr));
    free(ptr);
#else
    realptr = (char*)ptr-PREFIX_SIZE;
    oldsize = *((size_t*)realptr);
    update_zmalloc_stat_free(oldsize+PREFIX_SIZE);
    free(realptr);
#endif
}

char *zstrdup(const char *s) {
    size_t l = strlen(s)+1;
    char *p = zmalloc(l);

    memcpy(p,s,l); // 从s指针的起始指针开始拷贝l长度的数据到p指针指向的内存中
    return p;
}

size_t zmalloc_used_memory(void) {
    size_t um;

    if (zmalloc_thread_safe) {
#if defined(__ATOMIC_RELAXED) || defined(HAVE_ATOMIC)
        um = update_zmalloc_stat_add(0);
#else
        pthread_mutex_lock(&used_memory_mutex);
        um = used_memory;
        pthread_mutex_unlock(&used_memory_mutex);
#endif
    }
    else {
        um = used_memory;
    }

    return um;
}

void zmalloc_enable_thread_safeness(void) {
    zmalloc_thread_safe = 1;
}

void zmalloc_set_oom_handler(void (*oom_handler)(size_t)) {
    zmalloc_oom_handler = oom_handler;
}

/* Get the RSS information in an OS-specific way.
 *
 * WARNING: the function zmalloc_get_rss() is not designed to be fast
 * and may not be called in the busy loops where Redis tries to release
 * memory expiring or swapping out objects.
 *
 * For this kind of "fast RSS reporting" usages use instead the
 * function RedisEstimateRSS() that is a much faster (and less precise)
 * version of the function. */
// 如果操作系统使用/proc/${pid}/stat文件记录进程状态则会存在HAVE_PROC_STAT这个宏
// 那么采用第一种方式获取rss
#if defined(HAVE_PROC_STAT)
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

size_t zmalloc_get_rss(void) {
    int page = sysconf(_SC_PAGESIZE); // 获取操作系统内存分页,每页所占用的字节数
    size_t rss;
    char buf[4096];
    char filename[256];
    int fd, count;
    char *p, *x;
    // 动态生成linux系统下当前进程的stat文件的决对路径
    snprintf(filename,256,"/proc/%d/stat",getpid());
    // 以只读方式打开stat文件
    if ((fd = open(filename,O_RDONLY)) == -1) return 0;
    if (read(fd,buf,4096) <= 0) {
        close(fd);
        return 0;
    }
    close(fd);

    p = buf;
    // rss字段在stat文件的以\t分隔符分割的第24个字段,字段下标以0开始
    count = 23; /* RSS is the 24th field in /proc/<pid>/stat */
    while(p && count--) {
        p = strchr(p,' ');
        if (p) p++;
    }
    if (!p) return 0;
    x = strchr(p,' ');
    if (!x) return 0;
    *x = '\0';
    // 将以string类型获取的第24个字段的内容转换成long long类型
    rss = strtoll(p,NULL,10); //string to long long
    // 用rss*page的大小获得真实占用的内存的字节数
    rss *= page;
    return rss;
}
// 若操作系统内核使用的是mach,那么会存在HAVE_TASKINFO这个宏
// 采用第二种方式获取rss
#elif defined(HAVE_TASKINFO)
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#include <mach/task.h>
#include <mach/mach_init.h>
// mac系统通过获取当前进程
size_t zmalloc_get_rss(void) {
    task_t task = MACH_PORT_NULL;
    struct task_basic_info t_info;
    mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT; // 这个mach_msg_type_number_t类型其实是一个int类型

    // 通过当前进程pid获取当前task信息
    if (task_for_pid(current_task(), getpid(), &task) != KERN_SUCCESS)
        return 0;
    // 获取task_info信息
    task_info(task, TASK_BASIC_INFO, (task_info_t)&t_info, &t_info_count);

    return t_info.resident_size;
}
#else
// 若以上宏都没有,说明操作系统不支持获取真实的rss,使用zmalloc_used_memory()函数获取程序自己记录的memory使用的大小
size_t zmalloc_get_rss(void) {
    /* If we can't get the RSS in an OS-specific way for this system just
     * return the memory usage we estimated in zmalloc()..
     *
     * Fragmentation will appear to be always 1 (no fragmentation)
     * of course... */
    return zmalloc_used_memory();
}
#endif

/* Fragmentation = RSS / allocated-bytes */
float zmalloc_get_fragmentation_ratio(size_t rss) {
    return (float)rss/zmalloc_used_memory();
}

/* Get the sum of the specified field (converted form kb to bytes) in
 * /proc/self/smaps. The field must be specified with trailing ":" as it
 * apperas in the smaps output.
 *
 * Example: zmalloc_get_smap_bytes_by_field("Rss:");
 */
#if defined(HAVE_PROC_SMAPS)
size_t zmalloc_get_smap_bytes_by_field(char *field) {
    char line[1024];
    size_t bytes = 0;
    FILE *fp = fopen("/proc/self/smaps","r");
    int flen = strlen(field);

    if (!fp) return 0;
    while(fgets(line,sizeof(line),fp) != NULL) {
        if (strncmp(line,field,flen) == 0) {
            char *p = strchr(line,'k');
            if (p) {
                *p = '\0';
                bytes += strtol(line+flen,NULL,10) * 1024;
            }
        }
    }
    fclose(fp);
    return bytes;
}
#else
size_t zmalloc_get_smap_bytes_by_field(char *field) {
    ((void) field);
    return 0;
}
#endif

size_t zmalloc_get_private_dirty(void) {
    return zmalloc_get_smap_bytes_by_field("Private_Dirty:");
}
