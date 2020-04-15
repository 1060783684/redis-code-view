/* Hash Tables Implementation.
 *
 * This file implements in-memory hash tables with insert/del/replace/find/
 * get-random-element operations. Hash tables will auto-resize if needed
 * tables of power of two in size are used, collisions are handled by
 * chaining. See the source code for more information... :)
 *
 * Copyright (c) 2006-2012, Salvatore Sanfilippo <antirez at gmail dot com>
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

#include <stdint.h>

#ifndef __DICT_H
#define __DICT_H

#define DICT_OK 0
#define DICT_ERR 1

/* Unused arguments generate annoying warnings... */
#define DICT_NOTUSED(V) ((void) V)

// key-value对对象
typedef struct dictEntry {
    void *key;
    union {
        void *val; // 8Byte
        uint64_t u64; // 8Byte
        int64_t s64; // 8Byte
        double d; // 8Byte
    } v;
    struct dictEntry *next;
} dictEntry;

typedef struct dictType {
    unsigned int (*hashFunction)(const void *key); // 对key计算hash的函数指针
    void *(*keyDup)(void *privdata, const void *key); // 拷贝key的函数指针
    void *(*valDup)(void *privdata, const void *obj); // 拷贝value的函数指针
    int (*keyCompare)(void *privdata, const void *key1, const void *key2); // key比较的函数指针
    void (*keyDestructor)(void *privdata, void *key); // key析构函数指针
    void (*valDestructor)(void *privdata, void *obj); // value析构函数指针
} dictType;

/* This is our hash table structure. Every dictionary has two of this as we
 * implement incremental rehashing, for the old to the new table. */
// dict hash table
typedef struct dictht {
    dictEntry **table; // 存放数据用的表 *table指向一个dictEntry*数组的首地址,**table指向第一个dictEntry元素
    unsigned long size; // 总大小
    unsigned long sizemask; // hash掩码
    unsigned long used; // 已经使用的大小
} dictht;

typedef struct dict {
    dictType *type; // 各种操作key和val的函数指针结构体
    void *privdata; // ?
    dictht ht[2]; 
    long rehashidx; /* rehashing not in progress if rehashidx == -1 */
                    // 是否正在进行rehash,是的话存储下一个rehash项的index,否则为-1
    int iterators; /* number of iterators currently running */ // 运行中的安全迭代器数
} dict;

/* If safe is set to 1 this is a safe iterator, that means, you can call
 * dictAdd, dictFind, and other functions against the dictionary even while
 * iterating. Otherwise it is a non safe iterator, and only dictNext()
 * should be called while iterating. */
// dict迭代器,当safe为1时说明这是个进(线)程安全的迭代器
typedef struct dictIterator {
    dict *d;
    long index;
    int table, safe;
    dictEntry *entry, *nextEntry;
    /* unsafe iterator fingerprint for misuse detection. */
    // 用于误用检测的不安全迭代器指纹
    long long fingerprint;
} dictIterator;

typedef void (dictScanFunction)(void *privdata, const dictEntry *de);

/* This is the initial size of every hash table */
// hash表的初始大小
#define DICT_HT_INITIAL_SIZE     4

/* ------------------------------- Macros ------------------------------------*/
// dictFreeVal(dict d, dictEntry entry)
// 释放d中与entry匹配的dictEntry元素的val字段
#define dictFreeVal(d, entry) \
    if ((d)->type->valDestructor) \
        (d)->type->valDestructor((d)->privdata, (entry)->v.val)

// 将_val_设置到entry的val字段中,根据valDup函数指针的有无,判断是否需要使用valDup函数进行特殊定制化值拷贝
#define dictSetVal(d, entry, _val_) do { \
    if ((d)->type->valDup) \
        entry->v.val = (d)->type->valDup((d)->privdata, _val_); \
    else \
        entry->v.val = (_val_); \
} while(0)

// 设置entry中的value为val,类型为有符号long long
#define dictSetSignedIntegerVal(entry, _val_) \
    do { entry->v.s64 = _val_; } while(0)

// 设置entry中的value为val,类型为无符号long long
#define dictSetUnsignedIntegerVal(entry, _val_) \
    do { entry->v.u64 = _val_; } while(0)

// 设置entry中的value为val,类型为有符号double
#define dictSetDoubleVal(entry, _val_) \
    do { entry->v.d = _val_; } while(0)

// dictFreeKey(dict d, dictEntry entry)
// 释放d中与entry匹配的dictEntry元素的key字段
#define dictFreeKey(d, entry) \
    if ((d)->type->keyDestructor) \
        (d)->type->keyDestructor((d)->privdata, (entry)->key)

// 将key设置到entry的key字段中,根据keyDup函数指针的有无,判断是否需要使用keyDup函数进行特殊定制化值拷贝
#define dictSetKey(d, entry, _key_) do { \
    if ((d)->type->keyDup) \
        entry->key = (d)->type->keyDup((d)->privdata, _key_); \
    else \
        entry->key = (_key_); \
} while(0)

// 比较key1和key2的大小，若d->type->keyCompare函数指针有值，则使用keyCompare指向的函数进行比较，否则使用key1==key2进行比较
#define dictCompareKeys(d, key1, key2) \
    (((d)->type->keyCompare) ? \
        (d)->type->keyCompare((d)->privdata, key1, key2) : \
        (key1) == (key2))

#define dictHashKey(d, key) (d)->type->hashFunction(key) // 获取key的hash值，使用d->type->hashFunction指向的函数获取
#define dictGetKey(he) ((he)->key) // 获取dictEntry中的key值(值为指针类型)
#define dictGetVal(he) ((he)->v.val) // 获取dictEntry中的val值(值为指针类型)
#define dictGetSignedIntegerVal(he) ((he)->v.s64) // 以有符号longlong类型获取dictEntry中的v
#define dictGetUnsignedIntegerVal(he) ((he)->v.u64) // 以无符号longlong类型获取dictEntry中的v
#define dictGetDoubleVal(he) ((he)->v.d) // 以无符号double类型获取dictEntry中的v
#define dictSlots(d) ((d)->ht[0].size+(d)->ht[1].size) // 获取dict中两个hash table的总大小
#define dictSize(d) ((d)->ht[0].used+(d)->ht[1].used) // 获取dict中两个hash table已经使用的总大小
#define dictIsRehashing(d) ((d)->rehashidx != -1)

/* API */
dict *dictCreate(dictType *type, void *privDataPtr);
int dictExpand(dict *d, unsigned long size);
int dictAdd(dict *d, void *key, void *val);
dictEntry *dictAddRaw(dict *d, void *key);
int dictReplace(dict *d, void *key, void *val);
dictEntry *dictReplaceRaw(dict *d, void *key);
int dictDelete(dict *d, const void *key);
int dictDeleteNoFree(dict *d, const void *key);
void dictRelease(dict *d);
dictEntry * dictFind(dict *d, const void *key);
void *dictFetchValue(dict *d, const void *key);
int dictResize(dict *d);
dictIterator *dictGetIterator(dict *d);
dictIterator *dictGetSafeIterator(dict *d);
dictEntry *dictNext(dictIterator *iter);
void dictReleaseIterator(dictIterator *iter);
dictEntry *dictGetRandomKey(dict *d);
void dictPrintStats(dict *d);
unsigned int dictGenHashFunction(const void *key, int len);
unsigned int dictGenCaseHashFunction(const unsigned char *buf, int len);
void dictEmpty(dict *d, void(callback)(void*));
void dictEnableResize(void);
void dictDisableResize(void);
int dictRehash(dict *d, int n);
int dictRehashMilliseconds(dict *d, int ms);
void dictSetHashFunctionSeed(unsigned int initval); // seed(种子)
unsigned int dictGetHashFunctionSeed(void);
unsigned long dictScan(dict *d, unsigned long v, dictScanFunction *fn, void *privdata);

/* Hash table types */
extern dictType dictTypeHeapStringCopyKey;
extern dictType dictTypeHeapStrings;
extern dictType dictTypeHeapStringCopyKeyValue;

#endif /* __DICT_H */
