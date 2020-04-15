/* The ziplist is a specially encoded dually linked list that is designed
 * to be very memory efficient. It stores both strings and integer values,
 * where integers are encoded as actual integers instead of a series of
 * characters. It allows push and pop operations on either side of the list
 * in O(1) time. However, because every operation requires a reallocation of
 * the memory used by the ziplist, the actual complexity is related to the
 * amount of memory used by the ziplist.
 *
 * ziplist是一个特殊编码的双链表，旨在提高内存效率。 它存储字符串和整数值，其中整数被编码为实际整数而不是一系列字符。
 * 它允许在O（1）时间内在列表的任一侧进行push和pop操作。 但是，因为每个操作都需要重新分配ziplist使用的内存，
 * 所以实际的复杂性与ziplist使用的内存量有关。
 *
 * ----------------------------------------------------------------------------
 *
 * ZIPLIST OVERALL LAYOUT:
 * ZIPLIST总体布局：
 *
 * The general layout of the ziplist is as follows:
 * <zlbytes> <zltail> <zllen> <entry> <entry> <zlend>
 *
 * ziplist的总体布局如下：
 * <zlbytes><zltail><zllen><entry><entry><zlend>
 *
 * <zlbytes> is an unsigned integer to hold the number of bytes that the
 * ziplist occupies. This value needs to be stored to be able to resize the
 * entire structure without the need to traverse it first.
 *<zlbytes>是一个无符号整数，用于保存ziplist占用的字节数。 需要存储此值以便能够调整整个结构的大小，而无需先遍历它。
 *
 * <zltail> is the offset to the last entry in the list. This allows a pop
 * operation on the far side of the list without the need for full traversal.
 * <zltail>是列表中最后一个entry的偏移量。 这允许在列表的远端进行弹出操作，而无需完全遍历。
 *
 * <zllen> is the number of entries.When this value is larger than 2**16-2,
 * we need to traverse the entire list to know how many items it holds.
 * <zllen>是entry个数。当这个值大于2^16-2时，我们需要遍历整个列表以知道它拥有多少项。
 *
 * <zlend> is a single byte special value, equal to 255, which indicates the
 * end of the list.
 * <zlend>是单字节特殊值，等于255，表示列表的结尾。(一个魔数)
 *
 * ZIPLIST ENTRIES:
 * Every entry in the ziplist is prefixed by a header that contains two pieces
 * of information. First, the length of the previous entry is stored to be
 * able to traverse the list from back to front. Second, the encoding with an
 * optional string length of the entry itself is stored.
 * ziplist中的每个entry都以"含有两条信息的header"为前缀。首先，存储"前一个"entry的长度，以便能够从后到前遍历列表。
 * 其次，存储具有entry本身的可选字符串长度的编码。
 *
 * The length of the previous entry is encoded in the following way:
 * 上一个entry的长度按以下方式编码：
 *
 * If this length is smaller than 254 bytes, it will only consume a single
 * byte that takes the length as value. When the length is greater than or
 * equal to 254, it will consume 5 bytes. The first byte is set to 254 to
 * indicate a larger value is following. The remaining 4 bytes take the
 * length of the previous entry as value.
 * 如果此长度小于254个字节(不包括254)，则只用一个字节记录长度。当长度大于或等于254时，它将消耗5个字节。
 * 第一个字节设置为254，表示后面跟着一个更大的值。剩余的4个字节将前一个entry的长度作为值。
 * 小于254:比如长度为9
 * |<forward entry>|0000 1001|
 *
 * 大于等于254:比如长度为257
 * |<forward entry>|1111 1110|0000 0000|0000 0000|0000 0001|0000 0001|
 *
 * The other header field of the entry itself depends on the contents of the
 * entry. When the entry is a string, the first 2 bits of this header will hold
 * the type of encoding used to store the length of the string, followed by the
 * actual length of the string. When the entry is an integer the first 2 bits
 * are both set to 1. The following 2 bits are used to specify what kind of
 * integer will be stored after this header. An overview of the different
 * types and encodings is as follows:
 * entry本身的另一个标题字段取决于entry的内容。当entry是字符串时，此标头的前2位将保存用于存储字符串长度的编码类型，
 * 后跟字符串的实际长度。当entry是整数时，前2位都设置为1，接下来的2位用于指定在此标头之后将存储的整数类型。不同类型和编码的概述如下：
 *
 * |00pppppp| - 1 byte
 * |00实际长度| 总共占用一个字节
 *      String value with length less than or equal to 63 bytes (6 bits).
 *      字符串值，长度小于或等于63字节（6位）。
 * |01pppppp|qqqqqqqq| - 2 bytes
 * |01实际|长度| 总共占用两个字节
 *      String value with length less than or equal to 16383 bytes (14 bits).
 *      字符串值，长度小于或等于16383字节（14位）。
 * |10______|qqqqqqqq|rrrrrrrr|ssssssss|tttttttt| - 5 bytes
 * |10实|际|长|度|。。。| 总共占用5个字节
 *      String value with length greater than or equal to 16384 bytes.
 *      长度大于或等于16384字节的字符串值。
 * |1100 0000| - 1 byte
 *      Integer encoded as int16_t (2 bytes).
 *      整数编码为int16_t（2个字节）。
 * |1101 0000| - 1 byte
 *      Integer encoded as int32_t (4 bytes).
 *      整数编码为int32_t（4字节）。
 * |1110 0000| - 1 byte
 *      Integer encoded as int64_t (8 bytes).
 *      整数编码为int64_t（8字节）。
 * |1111 0000| - 1 byte
 *      Integer encoded as 24 bit signed (3 bytes).
 *      24位的有符号整数编码类型（3字节）。
 * |1111 1110| - 1 byte
 *      Integer encoded as 8 bit signed (1 byte).
 *      8位的有符号整数编码类型（1字节）。
 * |1111 xxxx| - (with xxxx between 0000 and 1101) immediate 4 bit integer. （xxxx在0000和1101之间）立即4位整数。
 *      Unsigned integer from 0 to 12. The encoded value is actually from
 *      1 to 13 because 0000 and 1111 can not be used, so 1 should be
 *      subtracted from the encoded 4 bit value to obtain the right value.
 *      0到12之间的无符号整数。编码值实际上是1到13，因为不能使用0000和1111，因此应从编码的4位值中减去1以获得正确的值。
 * |1111 1111| - End of ziplist. ziplist的结尾<zlend>的实际值
 *
 * All the integers are represented in little endian byte order.
 *
 * -------------------------------------以下为版权信息---------------------------------------
 *
 * Copyright (c) 2009-2012, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 * Copyright (c) 2009-2012, Salvatore Sanfilippo <antirez at gmail dot com>
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
#include <string.h>
#include <stdint.h>
#include <limits.h>
#include "zmalloc.h"
#include "util.h"
#include "ziplist.h"
#include "endianconv.h"
#include "redisassert.h"

#define ZIP_END 255
#define ZIP_BIGLEN 254

/* Different encoding/length possibilities */
#define ZIP_STR_MASK 0xc0 // |1100 0000|
#define ZIP_INT_MASK 0x30
#define ZIP_STR_06B (0 << 6) // |0000 0000|
#define ZIP_STR_14B (1 << 6) // |0100 0000|
#define ZIP_STR_32B (2 << 6) // |1000 0000|
#define ZIP_INT_16B (0xc0 | 0<<4) // 0xc0:|1100 0000|
#define ZIP_INT_32B (0xc0 | 1<<4) // |1101 0000|
#define ZIP_INT_64B (0xc0 | 2<<4) // |1110 0000|
#define ZIP_INT_24B (0xc0 | 3<<4) // |1111 0000|
#define ZIP_INT_8B 0xfe // |1111 1110|
/* 4 bit integer immediate encoding */
#define ZIP_INT_IMM_MASK 0x0f   /* 00001111 */ // 无符号整数长度编码的掩码
#define ZIP_INT_IMM_MIN 0xf1    /* 11110001 */ // 无符号整数长度编码的最小值
#define ZIP_INT_IMM_MAX 0xfd    /* 11111101 */ // 无符号整数长度编码的最大值
#define ZIP_INT_IMM_VAL(v) (v & ZIP_INT_IMM_MASK) // 通过无符号长度编码获取真实长度

#define INT24_MAX 0x7fffff
#define INT24_MIN (-INT24_MAX - 1)

/* Macro to determine type */
#define ZIP_IS_STR(enc) (((enc) & ZIP_STR_MASK) < ZIP_STR_MASK) // 通过entry头中的encode判断entry是不是str类型

/* Utility macros */
#define ZIPLIST_BYTES(zl)       (*((uint32_t*)(zl))) // 将zl强转成uint32_t*类型在解指针获取其中的内容,这里是获取ziplist的<zlbytes>部分的数据
#define ZIPLIST_TAIL_OFFSET(zl) (*((uint32_t*)((zl)+sizeof(uint32_t)))) // 获取ziplist的<zltail>部分的数据
#define ZIPLIST_LENGTH(zl)      (*((uint16_t*)((zl)+sizeof(uint32_t)*2))) // 获取ziplist的<zllen>部分的数据
#define ZIPLIST_HEADER_SIZE     (sizeof(uint32_t)*2+sizeof(uint16_t)) // ziplist的header size
                                                    // header包括<zlbytes>(32bit) <zltail>(32bit) <zllen>(16bit)
#define ZIPLIST_ENTRY_HEAD(zl)  ((zl)+ZIPLIST_HEADER_SIZE) // 获取ziplist的首个entry的首地址
#define ZIPLIST_ENTRY_TAIL(zl)  ((zl)+intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))) // 获取ziplist的最后一个entry的首地址
#define ZIPLIST_ENTRY_END(zl)   ((zl)+intrev32ifbe(ZIPLIST_BYTES(zl))-1) // 获取ziplist <zlend>的首地址

/* We know a positive increment can only be 1 because entries can only be
 * pushed one at a time. */
// 增加ziplist中length字段的值
// intrev16ifbe()转成小端字节序
#define ZIPLIST_INCR_LENGTH(zl,incr) { \
    if (ZIPLIST_LENGTH(zl) < UINT16_MAX) \
        ZIPLIST_LENGTH(zl) = intrev16ifbe(intrev16ifbe(ZIPLIST_LENGTH(zl))+incr); \
}

// ziplist entry 结构体,为使用便利用的封装形式,真实存放在ziplist中的是上面注释中描述的格式
typedef struct zlentry {
    unsigned int prevrawlensize, prevrawlen; // prevrawlensize:前一个entry的长度所占的字节大小;prevrawlen:前一个entry的长度
    unsigned int lensize, len; // lensize:自己的长度的所占的字节大小;len:自身的长度(自身有效负载所占的字节数),不包括头部的长度
    unsigned int headersize; // 头部所占的字节数
    unsigned char encoding; // 编码类型
    unsigned char *p; // 当前entry的首地址即entry的header的首地址
} zlentry;

/* Extract the encoding from the byte pointed by 'ptr' and set it into
 * 'encoding'. */
// 内部调用函数，将ptr指向的地址中的编码信息提取出来存放到变量'encoding'中
#define ZIP_ENTRY_ENCODING(ptr, encoding) do {  \
    (encoding) = (ptr[0]); \
    if ((encoding) < ZIP_STR_MASK) (encoding) &= ZIP_STR_MASK; \
} while(0)

/* Return bytes needed to store integer encoded by 'encoding' */
// 内部调用函数，根据encoding的内容判断entry的内容所占的字节数,encoding必须是整数类型的
static unsigned int zipIntSize(unsigned char encoding) {
    switch(encoding) {
    case ZIP_INT_8B:  return 1;
    case ZIP_INT_16B: return 2;
    case ZIP_INT_24B: return 3;
    case ZIP_INT_32B: return 4;
    case ZIP_INT_64B: return 8;
    default: return 0; /* 4 bit immediate */
    }
    assert(NULL);
    return 0;
}

/* Encode the length 'rawlen' writing it in 'p'. If p is NULL it just returns
 * the amount of bytes required to encode such a length. */
// 内部调用函数，将entry的类型长度编码(rawlen参数)存入p中，若p为NULL，则返回存放rawlen所需要的bit数
// 注：string的编码格式是'前缀'+'实际长度'需要多个字节,int的编码格式是只有'前缀'只需要一个字节
static unsigned int zipEncodeLength(unsigned char *p, unsigned char encoding, unsigned int rawlen) {
    unsigned char len = 1, buf[5];

    if (ZIP_IS_STR(encoding)) {
        /* Although encoding is given it may not be set for strings,
         * so we determine it here using the raw length. */
        if (rawlen <= 0x3f) { // |0011 1111|
            if (!p) return len;
            buf[0] = ZIP_STR_06B | rawlen;
        } else if (rawlen <= 0x3fff) { // |0011 1111 1111 1111|
            len += 1;
            if (!p) return len;
            buf[0] = ZIP_STR_14B | ((rawlen >> 8) & 0x3f); // 存放前8位的信息以及ZIP_STR_14B前缀
            buf[1] = rawlen & 0xff; // 存放后8位的信息
        } else {
            // 类似与else if
            len += 4;
            if (!p) return len;
            buf[0] = ZIP_STR_32B;
            buf[1] = (rawlen >> 24) & 0xff;
            buf[2] = (rawlen >> 16) & 0xff;
            buf[3] = (rawlen >> 8) & 0xff;
            buf[4] = rawlen & 0xff;
        }
    } else {
        /* Implies integer encoding, so length is always 1. */
        // 表示整数编码,所以长度始终为1
        if (!p) return len;
        buf[0] = encoding;
    }

    /* Store this length at p */
    // 将buf连续空间中的内容拷贝到p中
    memcpy(p,buf,len);
    return len;
}

/* Decode the length encoded in 'ptr'. The 'encoding' variable will hold the
 * entries encoding, the 'lensize' variable will hold the number of bytes
 * required to encode the entries length, and the 'len' variable will hold the
 * entries length. */
// 内部调用方法，解析ptr,将entry编码存入encoding变量(字符串或者整型),entry的'长度'所需要的字节数存入lensize,entry的长度存入len
// zipEncodeLength的逆操作
#define ZIP_DECODE_LENGTH(ptr, encoding, lensize, len) do {                    \
    ZIP_ENTRY_ENCODING((ptr), (encoding));                                     \
    // string编码
    if ((encoding) < ZIP_STR_MASK) {                                           \
        if ((encoding) == ZIP_STR_06B) {                                       \
            (lensize) = 1;                                                     \
            (len) = (ptr)[0] & 0x3f;                                           \
        } else if ((encoding) == ZIP_STR_14B) {                                \
            (lensize) = 2;                                                     \
            (len) = (((ptr)[0] & 0x3f) << 8) | (ptr)[1];                       \
        } else if (encoding == ZIP_STR_32B) {                                  \
            (lensize) = 5;                                                     \
            (len) = ((ptr)[1] << 24) |                                         \
                    ((ptr)[2] << 16) |                                         \
                    ((ptr)[3] <<  8) |                                         \
                    ((ptr)[4]);                                                \
        } else {                                                               \
            assert(NULL);                                                      \
        }                                                                      \
    // int编码
    } else {                                                                   \
        (lensize) = 1;                                                         \
        (len) = zipIntSize(encoding);                                          \
    }                                                                          \
} while(0);

/* Encode the length of the previous entry and write it to "p". Return the
 * number of bytes needed to encode this length if "p" is NULL. */
// 内部调用方法，编码前一个entry的长度并写入p,如果p是NULL则直接返回保存编码长度需要的字节数,小于254使用一个字节,大于254使用5个字节
static unsigned int zipPrevEncodeLength(unsigned char *p, unsigned int len) {
    if (p == NULL) {
        return (len < ZIP_BIGLEN) ? 1 : sizeof(len)+1; // 这里sizeof返回的是4,若len是malloc出来的,那么最小长度都得为size_t的长度
    } else {
        if (len < ZIP_BIGLEN) {
            p[0] = len;
            return 1;
        } else {
            p[0] = ZIP_BIGLEN;
            memcpy(p+1,&len,sizeof(len));
            memrev32ifbe(p+1);
            return 1+sizeof(len);
        }
    }
}

/* Encode the length of the previous entry and write it to "p". This only
 * uses the larger encoding (required in __ziplistCascadeUpdate). */
// 内部调用方法，存放前一个entry的长度编码，和zipPrevEncodeLength类似，但是这个方法只操作大于254长度即“大长度”编码
// 主要用来提供给__ziplistCascadeUpdate(级联更新)函数使用
static void zipPrevEncodeLengthForceLarge(unsigned char *p, unsigned int len) {
    if (p == NULL) return;
    p[0] = ZIP_BIGLEN;
    memcpy(p+1,&len,sizeof(len));
    memrev32ifbe(p+1);
}

/* Decode the number of bytes required to store the length of the previous
 * element, from the perspective of the entry pointed to by 'ptr'. */
// 内部调用方法，对ptr中的前一个entry的长度编码进行解码，获取存放长度编码所需要的字节数，并存放在prevlensize中
#define ZIP_DECODE_PREVLENSIZE(ptr, prevlensize) do {                          \
    if ((ptr)[0] < ZIP_BIGLEN) {                                               \
        (prevlensize) = 1;                                                     \
    } else {                                                                   \
        (prevlensize) = 5;                                                     \
    }                                                                          \
} while(0);

/* Decode the length of the previous element, from the perspective of the entry
 * pointed to by 'ptr'. */
// 内部调用方法，对ptr中的前一个entry的长度编码进行解码，获取存放长度编码所需要的字节数，并存放在prevlensize中，
// 获取前一个entry的实际长度并存放在prevlen中(获取prevlensize是通过ZIP_DECODE_PREVLENSIZE()方法做到的)
#define ZIP_DECODE_PREVLEN(ptr, prevlensize, prevlen) do {                     \
    ZIP_DECODE_PREVLENSIZE(ptr, prevlensize);                                  \
    if ((prevlensize) == 1) {                                                  \
        (prevlen) = (ptr)[0];                                                  \
    } else if ((prevlensize) == 5) {                                           \
        assert(sizeof((prevlensize)) == 4);                                    \
        memcpy(&(prevlen), ((char*)(ptr)) + 1, 4);                             \
        memrev32ifbe(&prevlen);                                                \
    }                                                                          \
} while(0);

/* Return the difference in number of bytes needed to store the length of the
 * previous element 'len', in the entry pointed to by 'p'. */
// 内部调用方法，返回存储p中保存的前一个entry长度编码所需要的长度与保存len编码后长度(例如：len大于254那么保存编码后的它就需要5个字节)的差
static int zipPrevLenByteDiff(unsigned char *p, unsigned int len) {
    unsigned int prevlensize;
    ZIP_DECODE_PREVLENSIZE(p, prevlensize);
    return zipPrevEncodeLength(NULL, len) - prevlensize;
}

/* Return the total number of bytes used by the entry pointed to by 'p'. */
// 内部调用方法，返回p所指向的entry的总长度，长度包含三个部分:
// 1. 前一个entry的长度编码的长度;2. 自身长度编码的长度;3. 自身内容的长度
static unsigned int zipRawEntryLength(unsigned char *p) {
    unsigned int prevlensize, encoding, lensize, len;
    ZIP_DECODE_PREVLENSIZE(p, prevlensize);
    ZIP_DECODE_LENGTH(p + prevlensize, encoding, lensize, len);
    return prevlensize + lensize + len;
}

/* Check if string pointed to by 'entry' can be encoded as an integer.
 * Stores the integer value in 'v' and its encoding in 'encoding'. */
// 内部调用函数，判断entry是否能编码位"整数",若能则将value存入参数v中,把编码存入encoding参数中
static int zipTryEncoding(unsigned char *entry, unsigned int entrylen, long long *v, unsigned char *encoding) {
    long long value;

    // int最大32位长
    if (entrylen >= 32 || entrylen == 0) return 0;
    // 尝试将string转换成long long类型
    if (string2ll((char*)entry,entrylen,&value)) {
        /* Great, the string can be encoded. Check what's the smallest
         * of our encoding types that can hold this value. */
        // 根据value所在的区间判断编码类型
        if (value >= 0 && value <= 12) {
            *encoding = ZIP_INT_IMM_MIN+value;
        } else if (value >= INT8_MIN && value <= INT8_MAX) {
            *encoding = ZIP_INT_8B;
        } else if (value >= INT16_MIN && value <= INT16_MAX) {
            *encoding = ZIP_INT_16B;
        } else if (value >= INT24_MIN && value <= INT24_MAX) {
            *encoding = ZIP_INT_24B;
        } else if (value >= INT32_MIN && value <= INT32_MAX) {
            *encoding = ZIP_INT_32B;
        } else {
            *encoding = ZIP_INT_64B;
        }
        *v = value;
        return 1;
    }
    return 0;
}

/* Store integer 'value' at 'p', encoded as 'encoding' */
// 内部调用函数，根据encoding判断整数编码类型，并将value存入p中
static void zipSaveInteger(unsigned char *p, int64_t value, unsigned char encoding) {
    int16_t i16;
    int32_t i32;
    int64_t i64;
    if (encoding == ZIP_INT_8B) {
        ((int8_t*)p)[0] = (int8_t)value;
    } else if (encoding == ZIP_INT_16B) {
        i16 = value;
        memcpy(p,&i16,sizeof(i16));
        memrev16ifbe(p);
    } else if (encoding == ZIP_INT_24B) {
        i32 = value<<8; //这里的左移8位和下面 memcpy的地址+1抵消了... 例子: 原始：00000000 00000000 00000000 11001100
                        // 左移8位：00000000 00000000 11001100 00000000
        memrev32ifbe(&i32); // 转成小端：00000000 11001100 00000000 00000000
        memcpy(p,((uint8_t*)&i32)+1,sizeof(i32)-sizeof(uint8_t)); // 地址+1：11001100 00000000 00000000
    } else if (encoding == ZIP_INT_32B) {
        i32 = value;
        memcpy(p,&i32,sizeof(i32));
        memrev32ifbe(p);
    } else if (encoding == ZIP_INT_64B) {
        i64 = value;
        memcpy(p,&i64,sizeof(i64));
        memrev64ifbe(p);
    } else if (encoding >= ZIP_INT_IMM_MIN && encoding <= ZIP_INT_IMM_MAX) {
        /* Nothing to do, the value is stored in the encoding itself. */
    } else {
        assert(NULL);
    }
}

/* Read integer encoded as 'encoding' from 'p' */
// 内部调用函数，zipSaveInteger的逆操作，根据encoding整数编码类型从p中获取int值
static int64_t zipLoadInteger(unsigned char *p, unsigned char encoding) {
    int16_t i16;
    int32_t i32;
    int64_t i64, ret = 0;
    if (encoding == ZIP_INT_8B) {
        ret = ((int8_t*)p)[0];
    } else if (encoding == ZIP_INT_16B) {
        memcpy(&i16,p,sizeof(i16));
        memrev16ifbe(&i16);
        ret = i16;
    } else if (encoding == ZIP_INT_32B) {
        memcpy(&i32,p,sizeof(i32));
        memrev32ifbe(&i32);
        ret = i32;
    } else if (encoding == ZIP_INT_24B) {
        i32 = 0;
        memcpy(((uint8_t*)&i32)+1,p,sizeof(i32)-sizeof(uint8_t));
        memrev32ifbe(&i32);
        ret = i32>>8;
    } else if (encoding == ZIP_INT_64B) {
        memcpy(&i64,p,sizeof(i64));
        memrev64ifbe(&i64);
        ret = i64;
    } else if (encoding >= ZIP_INT_IMM_MIN && encoding <= ZIP_INT_IMM_MAX) {
        ret = (encoding & ZIP_INT_IMM_MASK)-1;
    } else {
        assert(NULL);
    }
    return ret;
}

/* Return a struct with all information about an entry. */
// 内部调用方法，将p指向的zip entry解压成zlentry结构体
static zlentry zipEntry(unsigned char *p) {
    zlentry e;
    
    // 先对p进行解码，获取entry的第一个结构，pre-entry的长度
    ZIP_DECODE_PREVLEN(p, e.prevrawlensize, e.prevrawlen);
    // 修改指针位置，从第二个结构entry encoding首地址开始，获取第二个结构的信息
    ZIP_DECODE_LENGTH(p + e.prevrawlensize, e.encoding, e.lensize, e.len);
    e.headersize = e.prevrawlensize + e.lensize;
    e.p = p;
    return e;
}

/* Create a new empty ziplist. */
// 创建一个空的ziplist
unsigned char *ziplistNew(void) {
    unsigned int bytes = ZIPLIST_HEADER_SIZE+1; // ZIPLIST_HEADER_SIZE是header的长度,后面+的这个1是<zlend>的长度
    unsigned char *zl = zmalloc(bytes);
    // 设置<zlbytes>元素值
    ZIPLIST_BYTES(zl) = intrev32ifbe(bytes);
    // 设置<zltail>元素值,ZIPLIST_HEADER_SIZE值正好是<zlend>的前一个位置，即tail的offset
    ZIPLIST_TAIL_OFFSET(zl) = intrev32ifbe(ZIPLIST_HEADER_SIZE);
    // 设置<zllen>元素值,空List的长度为0
    ZIPLIST_LENGTH(zl) = 0;
    // 设置<zlend>的"魔数"
    zl[bytes-1] = ZIP_END;
    return zl;
}

/* Resize the ziplist. */
// 内部调用函数，重制ziplist的长度，在插入元素或删除元素时用于重制长度
static unsigned char *ziplistResize(unsigned char *zl, unsigned int len) {
    zl = zrealloc(zl,len);
    ZIPLIST_BYTES(zl) = intrev32ifbe(len);
    zl[len-1] = ZIP_END;
    return zl;
}

/* When an entry is inserted, we need to set the prevlen field of the next
 * entry to equal the length of the inserted entry. It can occur that this
 * length cannot be encoded in 1 byte and the next entry needs to be grow
 * a bit larger to hold the 5-byte encoded prevlen. This can be done for free,
 * because this only happens when an entry is already being inserted (which
 * causes a realloc and memmove). However, encoding the prevlen may require
 * that this entry is grown as well. This effect may cascade throughout
 * the ziplist when there are consecutive entries with a size close to
 * ZIP_BIGLEN, so we need to check that the prevlen can be encoded in every
 * consecutive entry.
 *
 * 插入条目时，我们需要将下一个条目的prevlen字段设置为等于插入条目的长度。
 * 可能会发生此长度无法以1字节编码的情况，并且下一个条目需要增大一点以容纳5字节编码的prevlen。
 * 这可以免费完成，因为这仅在已插入条目时发生（这会导致重新分配和记忆）。
 * 但是，编码prevlen可能还需要增加该条目。
 * 当存在大小接近ZIP_BIGLEN的连续条目时，此效果可能会遍及整个ziplist，因此我们需要检查prevlen是否可以在每个连续条目中进行编码。
 *
 * Note that this effect can also happen in reverse, where the bytes required
 * to encode the prevlen field can shrink. This effect is deliberately ignored,
 * because it can cause a "flapping" effect where a chain prevlen fields is
 * first grown and then shrunk again after consecutive inserts. Rather, the
 * field is allowed to stay larger than necessary, because a large prevlen
 * field implies the ziplist is holding large entries anyway.
 *
 * 请注意，这种效果也可以反向发生，其中编码prevlen字段所需的字节可能会缩小。
 * 故意忽略此效果，因为它会导致“拍打”效果，其中先生成链前字段，然后在连续插入后再次缩小。
 * 而是允许该字段保持大于必要的大小，因为较大的prevlen字段表示该ziplist无论如何都包含较大的条目。
 *
 * The pointer "p" points to the first entry that does NOT need to be
 * updated, i.e. consecutive fields MAY need an update.
 *
 * 指针“ p”指向不需要更新的第一个条目，即连续字段可能需要更新。
 */
// ziplist级联更新
// 参数p新插入的节点的首地址
static unsigned char *__ziplistCascadeUpdate(unsigned char *zl, unsigned char *p) {
    size_t curlen = intrev32ifbe(ZIPLIST_BYTES(zl)), rawlen, rawlensize;
    size_t offset, noffset, extra;
    unsigned char *np;
    zlentry cur, next;

    // 遍历zl中的所有节点,当当前节点的长度和next节点的prerawlen长度不同时进行更新
    // 这个循环体最多执行两次
    while (p[0] != ZIP_END) {
        cur = zipEntry(p);
        rawlen = cur.headersize + cur.len;
        rawlensize = zipPrevEncodeLength(NULL,rawlen);

        /* Abort if there is no next entry. */
        // 如果当前节点的下一个位置是ZIP_END,说明新节点是最后一个节点,不需要级联更新,直接退出循环
        if (p[rawlen] == ZIP_END) break;
        // 如果不是最后一个节点则获取下一个节点的首地址
        next = zipEntry(p+rawlen);

        /* Abort when "prevlen" has not changed. */
        // 如果下一个节点的"前一个节点的长度"字段和这个新节点的长度相同,则不用更新,直接退出循环
        if (next.prevrawlen == rawlen) break;
        
        if (next.prevrawlensize < rawlensize) {
            /* The "prevlen" field of "next" needs more bytes to hold
             * the raw length of "cur". */
            // 用当前节点的首地址减去ziplist的首地址可以获得当前节点相对于ziplist首地址的相对位置offset
            offset = p-zl;
            // 获取多出来的长度
            extra = rawlensize-next.prevrawlensize;
            // 对ziplist进行扩容,增加extra长度
            zl = ziplistResize(zl,curlen+extra);
            // 获取新的ziplist中的当前节点的地址(resize后ziplist的首地址可能会改变)
            p = zl+offset;

            /* Current pointer and offset for next element. */
            // 获取next节点的新地址
            np = p+rawlen;
            // 获取next节点的offset
            noffset = np-zl;

            /* Update tail offset when next element is not the tail element. */
            // 当下一个节点不是tail时,更新ziplist中的tail的偏移量,这里只是更新偏移量,不会移动实际节点
            if ((zl+intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))) != np) {
                ZIPLIST_TAIL_OFFSET(zl) =
                    intrev32ifbe(intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))+extra);
            }

            /* Move the tail to the back. */
            // memmove(dest, src, size):将src开头,大小为size的内容复制到dest开头的位置src+szie的空间可以与dest+size的空间有重叠
            // 扩展next节点的header大小
            memmove(np+rawlensize,
                np+next.prevrawlensize,
                curlen-noffset-next.prevrawlensize-1); // 这里的-1是为了抛去ZIPEND那一位
            // 将新的前一个entry的长度编码填充到next entry的header中
            zipPrevEncodeLength(np,rawlen);

            /* Advance the cursor */
            p += rawlen;
            curlen += extra;
        } else {
            if (next.prevrawlensize > rawlensize) {
                /* This would result in shrinking, which we want to avoid.
                 * So, set "rawlen" in the available bytes. */
                // 当next.prevrawlensize大于rawlensize会导致next entry的header长度变小,但是这是需要避免的情况,
                // 因为这之后再插入或删除元素可能还会再进行扩容,这种情况被称为"flapping(拍打)"效应,这会影响插入和删除的速度,
                // 所以这种情况不会进行缩容(ZIP_BIGLEN要存小的长度也是可以存的),即entry的header的扩容是不可逆的
                // 用ZIP_BIGLEN存小长度的例子:长度9 ---> |1111 1110|0000 0000|0000 0000|0000 0000|0000 1001|
                zipPrevEncodeLengthForceLarge(p+rawlen,rawlen);
            } else {
                // 这里是next.prevrawlensize == rawlensize的情况,虽然存放长度的size相同,但是长度值本身不同,需要设置一下
                zipPrevEncodeLength(p+rawlen,rawlen);
            }

            /* Stop here, as the raw length of "next" has not changed. */
            break;
        }
    }
    return zl;
}

/* Delete "num" entries, starting at "p". Returns pointer to the ziplist. */
// 删除ziplist中从指针p开始的num个entry
static unsigned char *__ziplistDelete(unsigned char *zl, unsigned char *p, unsigned int num) {
    unsigned int i, totlen, deleted = 0;
    size_t offset;
    int nextdiff = 0;
    zlentry first, tail;

    first = zipEntry(p);
    for (i = 0; p[0] != ZIP_END && i < num; i++) {
        p += zipRawEntryLength(p);
        deleted++;
    }

    totlen = p-first.p; // 要删除的内容的总长度(字节数)
    if (totlen > 0) {
        // 要删除的所有节点的下一个地址不是ZIPEND时
        if (p[0] != ZIP_END) {
            /* Storing `prevrawlen` in this entry may increase or decrease the
             * number of bytes required compare to the current `prevrawlen`.
             * There always is room to store this, because it was previously
             * stored by an entry that is now being deleted. */
            // 获取从first开始第num+1个节点的prevrawlen与first.prevrawlen的差值
            // 若first.prevrawlen比prevrawlen长,则返回一个负数可能的返回值有4,0,-4
            nextdiff = zipPrevLenByteDiff(p,first.prevrawlen);
            // 修正“第num+1个节点”的首地址位置
            p -= nextdiff;
            // 修正“第num+1个节点”的prevrawlen值
            zipPrevEncodeLength(p,first.prevrawlen);

            /* Update offset for tail */
            // 修正ziplist的tail节点的offset值
            ZIPLIST_TAIL_OFFSET(zl) =
                intrev32ifbe(intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))-totlen);

            /* When the tail contains more than one entry, we need to take
             * "nextdiff" in account as well. Otherwise, a change in the
             * size of prevlen doesn't have an effect on the *tail* offset. */
            // 如果tail不是最后一个条目时上面的-totlen无法正确修正tail的offset,还需要考虑nextdiff值
            tail = zipEntry(p);
            if (p[tail.headersize+tail.len] != ZIP_END) {
                ZIPLIST_TAIL_OFFSET(zl) =
                   intrev32ifbe(intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))+nextdiff);
            }

            /* Move tail to the front of the ziplist */
            // 将最后一个需要删除的节点的后一个节点至ZIPEND位置的内容拷贝到first节点的首地址
            memmove(first.p,p,
                intrev32ifbe(ZIPLIST_BYTES(zl))-(p-zl)-1);
        } else { // 要删除的所有节点的下一个地址是ZIPEND时
            /* The entire tail was deleted. No need to move memory. */
            ZIPLIST_TAIL_OFFSET(zl) =
                intrev32ifbe((first.p-zl)-first.prevrawlen);
        }

        /* Resize and update length */
        offset = first.p-zl;
        // 重制ziplist的长度
        zl = ziplistResize(zl, intrev32ifbe(ZIPLIST_BYTES(zl))-totlen+nextdiff);
        // 修正<ziplen>字段值,记录entry的个数
        ZIPLIST_INCR_LENGTH(zl,-deleted);
        p = zl+offset;

        /* When nextdiff != 0, the raw length of the next entry has changed, so
         * we need to cascade the update throughout the ziplist */
        // 当nextdiff != 0时“从first开始第num+1个节点”的实际长度已更改(prevrawlensize变大或变小),所以需要级联更新
        if (nextdiff != 0)
            zl = __ziplistCascadeUpdate(zl,p);
    }
    return zl;
}

/* Insert item at "p". */
// 将item插入p所指向的位置
static unsigned char *__ziplistInsert(unsigned char *zl, unsigned char *p, unsigned char *s, unsigned int slen) {
    // reqlen:新entry的整体长度,包含三个部分,prerawsize、rawsize、contentsize
    size_t curlen = intrev32ifbe(ZIPLIST_BYTES(zl)), reqlen;
    unsigned int prevlensize, prevlen = 0;
    size_t offset;
    int nextdiff = 0;
    unsigned char encoding = 0;
    long long value = 123456789; /* initialized to avoid warning. Using a value
                                    that is easy to see if for some reason
                                    we use it uninitialized. */ // 初始化,这个初始值没有什么用
    zlentry tail;

    /* Find out prevlen for the entry that is inserted. */
    // 获取参数p的前一个entry的长度;若参数p指向的是ZIPEND则前一个元素就是ziptail,获取ziptail的长度即可
    if (p[0] != ZIP_END) {
        ZIP_DECODE_PREVLEN(p, prevlensize, prevlen);
    } else {
        unsigned char *ptail = ZIPLIST_ENTRY_TAIL(zl);
        if (ptail[0] != ZIP_END) {
            prevlen = zipRawEntryLength(ptail);
        }
    }

    /* See if the entry can be encoded */
    // 查看条目是否可以进行int编码(即查看此string是否能转成int),若可以则使用int编码,长度也是用int编码对应的长度
    if (zipTryEncoding(s,slen,&value,&encoding)) {
        /* 'encoding' is set to the appropriate integer encoding */
        reqlen = zipIntSize(encoding);
    } else {
        /* 'encoding' is untouched, however zipEncodeLength will use the
         * string length to figure out how to encode it. */
        reqlen = slen;
    }
    /* We need space for both the length of the previous entry and
     * the length of the payload. */
    // 需要分配额外的空间存放上一个entry的长度,以及自身编码的类型
    reqlen += zipPrevEncodeLength(NULL,prevlen);
    reqlen += zipEncodeLength(NULL,encoding,slen);

    /* When the insert position is not equal to the tail, we need to
     * make sure that the next entry can hold this entry's length in
     * its prevlen field. */
    // 当新插入的这个元素不是最后一个元素时,需要保证下一个元素的prerawlen字段能存下新元素的长度
    // 若新字符串的长度比p.prerawlen的长度长则返回正数
    nextdiff = (p[0] != ZIP_END) ? zipPrevLenByteDiff(p,reqlen) : 0;

    /* Store offset because a realloc may change the address of zl. */
    // 获取p的offset
    offset = p-zl;
    // 重新分配ziplist的空间,这里不会因为nextdiff是负数而导致丢数据,因为reqlen一定会大于nextdiff
    zl = ziplistResize(zl,curlen+reqlen+nextdiff);
    // 重新获取p的地址
    p = zl+offset;

    /* Apply memory move when necessary and update tail offset. */
    // 若p是ZIPEND,则新条目一点是ziplist的tail只需要更改zip_tail_offet
    // 否则需要移动ziplist的内容
    if (p[0] != ZIP_END) {
        /* Subtract one because of the ZIP_END bytes */
        // reqlen存放的是整个entry的长度包括header size和有效负载的长度
        // 将p开始到ZIPEND首地址的内容拷贝至新位置,腾出一个新entry的空间
        memmove(p+reqlen,p-nextdiff,curlen-offset-1+nextdiff);

        /* Encode this entry's raw length in the next entry. */
        // 将新entry的长度编码后存入p的prerawlen字段中
        zipPrevEncodeLength(p+reqlen,reqlen);

        /* Update offset for tail */
        // 修正ziplist_tail的偏移量
        ZIPLIST_TAIL_OFFSET(zl) =
            intrev32ifbe(intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))+reqlen);

        /* When the tail contains more than one entry, we need to take
         * "nextdiff" in account as well. Otherwise, a change in the
         * size of prevlen doesn't have an effect on the *tail* offset. */
        
        // 更新tail的offset
        tail = zipEntry(p+reqlen);
        if (p[reqlen+tail.headersize+tail.len] != ZIP_END) {
            ZIPLIST_TAIL_OFFSET(zl) =
                intrev32ifbe(intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))+nextdiff);
        }
    } else {
        /* This element will be the new tail. */
        ZIPLIST_TAIL_OFFSET(zl) = intrev32ifbe(p-zl);
    }

    /* When nextdiff != 0, the raw length of the next entry has changed, so
     * we need to cascade the update throughout the ziplist */
    // 当nextdiff != 0时next节点的原始长度会产生变化(prerawlen会变化),需要进行级联更新
    if (nextdiff != 0) {
        offset = p-zl;
        zl = __ziplistCascadeUpdate(zl,p+reqlen);
        p = zl+offset;
    }

    /* Write the entry */
    // 这时候p的位置已经是新entry应该存放的位置
    // 先存入前一个enrty的长度编码
    p += zipPrevEncodeLength(p,prevlen);
    // 存放本entry的编码
    p += zipEncodeLength(p,encoding,slen);
    // 根据编码类型存放content,有两种编码类型,整型和字符串
    if (ZIP_IS_STR(encoding)) {
        memcpy(p,s,slen);
    } else {
        zipSaveInteger(p,value,encoding);
    }
    // 对<ziplen>进行增一操作,因为ziplist的entry个数增加了一个
    ZIPLIST_INCR_LENGTH(zl,1);
    return zl;
}

// 对外提供的函数，往ziplist里push一个元素，可以往tail的位置push，也可以往head的位置push
unsigned char *ziplistPush(unsigned char *zl, unsigned char *s, unsigned int slen, int where) {
    unsigned char *p;
    // 根据where判断是往head的位置push，还是往tail的位置push
    p = (where == ZIPLIST_HEAD) ? ZIPLIST_ENTRY_HEAD(zl) : ZIPLIST_ENTRY_END(zl);
    // 调用上面的那个插入函数进行插入操作
    return __ziplistInsert(zl,p,s,slen);
}

/* Returns an offset to use for iterating with ziplistNext. When the given
 * index is negative, the list is traversed back to front. When the list
 * doesn't contain an element at the provided index, NULL is returned. */
// 对外提供的函数，返回指向ziplist中第index个元素的指针
// 若index为负数,则从后往前获取-index个元素的指针
// 当index个元素不存在时,返回NULL
unsigned char *ziplistIndex(unsigned char *zl, int index) {
    unsigned char *p;
    unsigned int prevlensize, prevlen = 0;
    // 当index小于0时从后往前遍历
    if (index < 0) {
        index = (-index)-1;
        p = ZIPLIST_ENTRY_TAIL(zl);
        if (p[0] != ZIP_END) {
            ZIP_DECODE_PREVLEN(p, prevlensize, prevlen);
            while (prevlen > 0 && index--) {
                p -= prevlen;
                ZIP_DECODE_PREVLEN(p, prevlensize, prevlen);
            }
        }
    }
    // 当index大于0时从前往后遍历
    else {
        p = ZIPLIST_ENTRY_HEAD(zl);
        while (p[0] != ZIP_END && index--) {
            p += zipRawEntryLength(p);
        }
    }
    // index没次循环都会减一,当最后index还大于0,说明index超出ziplist的entry个数了
    return (p[0] == ZIP_END || index > 0) ? NULL : p;
}

/* Return pointer to next entry in ziplist.
 *
 * zl is the pointer to the ziplist
 * p is the pointer to the current element
 *
 * The element after 'p' is returned, otherwise NULL if we are at the end. */
// 对外提供的函数，返回ziplist中p所指向的entry的下一个enrty,如果不存在则返回NULL
unsigned char *ziplistNext(unsigned char *zl, unsigned char *p) {
    ((void) zl);

    /* "p" could be equal to ZIP_END, caused by ziplistDelete,
     * and we should return NULL. Otherwise, we should return NULL
     * when the *next* element is ZIP_END (there is no next entry). */
    // 如果p指向zipend，则返回NULL
    if (p[0] == ZIP_END) {
        return NULL;
    }

    // 获取下一个元素的首地址,即当前元素的尾地址的后一个地址
    p += zipRawEntryLength(p);
    // 若新地址指向zipend，则返回NULL
    if (p[0] == ZIP_END) {
        return NULL;
    }

    return p;
}

/* Return pointer to previous entry in ziplist. */
// 对外提供的函数，返回ziplist中p所指向的entry的前一个entry,若不存在则返回NULL
unsigned char *ziplistPrev(unsigned char *zl, unsigned char *p) {
    unsigned int prevlensize, prevlen = 0;

    /* Iterating backwards from ZIP_END should return the tail. When "p" is
     * equal to the first element of the list, we're already at the head,
     * and should return NULL. */
    // 若p指向zipend，则前一个元素一定是tail，直接返回当前ziplist的tail元素即可
    if (p[0] == ZIP_END) {
        p = ZIPLIST_ENTRY_TAIL(zl);
        return (p[0] == ZIP_END) ? NULL : p;
    }
    // 若p指向ziphead，则一定没有前一个元素，直接返回NULL
    else if (p == ZIPLIST_ENTRY_HEAD(zl)) {
        return NULL;
    } else {
        // 获取前一个entry的长度
        ZIP_DECODE_PREVLEN(p, prevlensize, prevlen);
        assert(prevlen > 0);
        // 用当前entry的首地址减去前一个entry的长度就能获得前一个entry的首地址
        return p-prevlen;
    }
}

/* Get entry pointed to by 'p' and store in either '*sstr' or 'sval' depending
 * on the encoding of the entry. '*sstr' is always set to NULL to be able
 * to find out whether the string pointer or the integer value was set.
 * Return 0 if 'p' points to the end of the ziplist, 1 otherwise. */
// 对外提供的函数，将ziplist中p所指向的entry的有效负载的首地址存入sstr或sval中，即抛去header后的内容的首地址，若entry的encode类型为str则存放在sstr中，若为int则存放在sval中;sstr初始化值为NULL，用于在方法调用完后判断是应该从sstr中取内容还是从sval中取内容
unsigned int ziplistGet(unsigned char *p, unsigned char **sstr, unsigned int *slen, long long *sval) {
    zlentry entry;
    if (p == NULL || p[0] == ZIP_END) return 0;
    if (sstr) *sstr = NULL;

    entry = zipEntry(p);
    if (ZIP_IS_STR(entry.encoding)) {
        if (sstr) {
            *slen = entry.len;
            *sstr = p+entry.headersize;
        }
    } else {
        if (sval) {
            *sval = zipLoadInteger(p+entry.headersize,entry.encoding);
        }
    }
    return 1;
}

/* Insert an entry at "p". */
// 对外提供的函数，将字符串s插入到ziplist中p所指向的位置，slen是字符串s的长度，直接调用了内部的__ziplistInsert函数实现，没有任何其他逻辑
unsigned char *ziplistInsert(unsigned char *zl, unsigned char *p, unsigned char *s, unsigned int slen) {
    return __ziplistInsert(zl,p,s,slen);
}

/* Delete a single entry from the ziplist, pointed to by *p.
 * Also update *p in place, to be able to iterate over the
 * ziplist, while deleting entries. */
// 对外提供的函数，删除ziplist中*p所指向的entry，并将*p所指向的entry的下一个entry的地址返回，
// 并返回新的ziplist的首地址(因为删除元素后需要resize，ziplist的首地址可能会改变)
unsigned char *ziplistDelete(unsigned char *zl, unsigned char **p) {
    size_t offset = *p-zl;
    zl = __ziplistDelete(zl,*p,1);

    /* Store pointer to current element in p, because ziplistDelete will
     * do a realloc which might result in a different "zl"-pointer.
     * When the delete direction is back to front, we might delete the last
     * entry and end up with "p" pointing to ZIP_END, so check this. */
    *p = zl+offset;
    return zl;
}

/* Delete a range of entries from the ziplist. */
// 对外提供的函数，删除ziplist中一个范围内的所有entry，即删除第index个entry开始的num个entry
unsigned char *ziplistDeleteRange(unsigned char *zl, unsigned int index, unsigned int num) {
    // 获取第index个元素的首地址，index可以为负数，为负数，则从后往前数index个，若index个元素不存在则返回NULL
    // 但是index的正负不会影响delete函数删除元素的方向，无论index是正还是负都是删除从index开始往后的num个元素
    unsigned char *p = ziplistIndex(zl,index);
    return (p == NULL) ? zl : __ziplistDelete(zl,p,num);
}

/* Compare entry pointer to by 'p' with 'sstr' of length 'slen'. */
/* Return 1 if equal. */
// 对外提供的函数，将p所指向的entry的有效负载和长度为slen的*sstr进行比较，若相等则返回1
unsigned int ziplistCompare(unsigned char *p, unsigned char *sstr, unsigned int slen) {
    zlentry entry;
    unsigned char sencoding;
    long long zval, sval;
    if (p[0] == ZIP_END) return 0;

    entry = zipEntry(p);
    // 先判断p所指向的entry的编码类型，若为str类型则进行string的equals比较
    if (ZIP_IS_STR(entry.encoding)) {
        /* Raw compare */
        if (entry.len == slen) {
            return memcmp(p+entry.headersize,sstr,slen) == 0;
        } else {
            return 0;
        }
    }
    // 若为int类型则尝试将sstr进行int转换，若能转换成功，将转换后的int值存入sval，再将p所指向的有效负载以int类型取出放入zval，并和sval进行比较
    else {
        /* Try to compare encoded values. Don't compare encoding because
         * different implementations may encoded integers differently. */
        if (zipTryEncoding(sstr,slen,&sval,&sencoding)) {
          zval = zipLoadInteger(p+entry.headersize,entry.encoding);
          return zval == sval;
        }
    }
    return 0;
}

/* Find pointer to the entry equal to the specified entry. Skip 'skip' entries
 * between every comparison. Returns NULL when the field could not be found. */
// 对外提供的函数，返回ziplist中从p开始往后的第一个有效负载和vstr相等的entry的首地址，每次遍历跳过skip个元素
unsigned char *ziplistFind(unsigned char *p, unsigned char *vstr, unsigned int vlen, unsigned int skip) {
    int skipcnt = 0;
    unsigned char vencoding = 0;
    long long vll = 0;

    while (p[0] != ZIP_END) {
        unsigned int prevlensize, encoding, lensize, len;
        unsigned char *q;

        // 获取p指向的entry的前一个entry的长度的大小
        ZIP_DECODE_PREVLENSIZE(p, prevlensize);
        // 获取p所指向的entry的encoding和有效负载的长度(len)
        ZIP_DECODE_LENGTH(p + prevlensize, encoding, lensize, len);
        q = p + prevlensize + lensize;
        
        // 判断是否需要跳过这个entry
        if (skipcnt == 0) {
            /* Compare current entry with specified entry */
            // 若编码类型为string则进行string类型的判断
            if (ZIP_IS_STR(encoding)) {
                if (len == vlen && memcmp(q, vstr, vlen) == 0) {
                    return p;
                }
            }
            // 否则进行整型类型的比较
            else {
                /* Find out if the searched field can be encoded. Note that
                 * we do it only the first time, once done vencoding is set
                 * to non-zero and vll is set to the integer value. */
                // 尝试对vstr进行整型编码，若不能编码，则进入下一次循环
                if (vencoding == 0) {
                    if (!zipTryEncoding(vstr, vlen, &vll, &vencoding)) {
                        /* If the entry can't be encoded we set it to
                         * UCHAR_MAX so that we don't retry again the next
                         * time. */
                        // 若int编码失败，则设置标志位
                        vencoding = UCHAR_MAX;
                    }
                    /* Must be non-zero by now */
                    assert(vencoding);
                }

                /* Compare current entry with specified entry, do it only
                 * if vencoding != UCHAR_MAX because if there is no encoding
                 * possible for the field it can't be a valid integer. */
                // 若置了标志位则不进行实际值比较
                if (vencoding != UCHAR_MAX) {
                    long long ll = zipLoadInteger(q, encoding);
                    if (ll == vll) {
                        return p;
                    }
                }
            }

            /* Reset skip count */
            // 每次比较完都重制一下skipcnt,每次遍历都自减一次,自减至0则进行判断
            skipcnt = skip;
        } else {
            /* Skip entry */
            skipcnt--;
        }

        /* Move to next entry */
        p = q + len;
    }

    return NULL;
}

/* Return length of ziplist. */
// 对外提供的函数，返回ziplist中entry的个数，即list的长度，若entry的个数小于2^16，则直接返回ziplen的值，否则需要遍历一遍ziplist获取entry的个数
unsigned int ziplistLen(unsigned char *zl) {
    unsigned int len = 0;
    // 判断ziplen的值是否小于2^16，若小于，则直接将ziplen的值返回回去
    if (intrev16ifbe(ZIPLIST_LENGTH(zl)) < UINT16_MAX) {
        len = intrev16ifbe(ZIPLIST_LENGTH(zl));
    }
    // 否则，将ziplist的entry遍历一遍获取实际个数
    else {
        unsigned char *p = zl+ZIPLIST_HEADER_SIZE;
        while (*p != ZIP_END) {
            p += zipRawEntryLength(p);
            len++;
        }

        /* Re-store length if small enough */
        // 若实际个数其实小于2^16=65535，则修正ziplen的内容
        if (len < UINT16_MAX) ZIPLIST_LENGTH(zl) = intrev16ifbe(len);
    }
    return len;
}

/* Return ziplist blob size in bytes. */
// 对外提供的函数，获取整个ziplist的长度，包括entries以及entries以外的其他部分的总长度
size_t ziplistBlobLen(unsigned char *zl) {
    return intrev32ifbe(ZIPLIST_BYTES(zl));
}

// 对外提供的函数，打印ziplist的信息到标准输出 。。。 待解析
void ziplistRepr(unsigned char *zl) {
    unsigned char *p;
    int index = 0;
    zlentry entry;

    printf(
        "{total bytes %d} "
        "{length %u}\n"
        "{tail offset %u}\n",
        intrev32ifbe(ZIPLIST_BYTES(zl)),
        intrev16ifbe(ZIPLIST_LENGTH(zl)),
        intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl)));
    p = ZIPLIST_ENTRY_HEAD(zl);
    while(*p != ZIP_END) {
        entry = zipEntry(p);
        printf(
            "{"
                "addr 0x%08lx, "
                "index %2d, "
                "offset %5ld, "
                "rl: %5u, "
                "hs %2u, "
                "pl: %5u, "
                "pls: %2u, "
                "payload %5u"
            "} ",
            (long unsigned)p,
            index,
            (unsigned long) (p-zl),
            entry.headersize+entry.len, // entry的长度
            entry.headersize, // header的长度
            entry.prevrawlen, // 前一个entry的长度
            entry.prevrawlensize, // 前一个entry长度所占用空间的长度
            entry.len); // 有效负载的长度
        p += entry.headersize;
        if (ZIP_IS_STR(entry.encoding)) {
            if (entry.len > 40) {
                if (fwrite(p,40,1,stdout) == 0) perror("fwrite");
                printf("...");
            } else {
                if (entry.len &&
                    fwrite(p,entry.len,1,stdout) == 0) perror("fwrite");
            }
        } else {
            printf("%lld", (long long) zipLoadInteger(p,entry.encoding));
        }
        printf("\n");
        p += entry.len;
        index++;
    }
    printf("{end}\n\n");
}

#ifdef ZIPLIST_TEST_MAIN
#include <sys/time.h>
#include "adlist.h"
#include "sds.h"

#define debug(f, ...) { if (DEBUG) printf(f, __VA_ARGS__); }

unsigned char *createList() {
    unsigned char *zl = ziplistNew();
    zl = ziplistPush(zl, (unsigned char*)"foo", 3, ZIPLIST_TAIL);
    zl = ziplistPush(zl, (unsigned char*)"quux", 4, ZIPLIST_TAIL);
    zl = ziplistPush(zl, (unsigned char*)"hello", 5, ZIPLIST_HEAD);
    zl = ziplistPush(zl, (unsigned char*)"1024", 4, ZIPLIST_TAIL);
    return zl;
}

unsigned char *createIntList() {
    unsigned char *zl = ziplistNew();
    char buf[32];

    sprintf(buf, "100");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_TAIL);
    sprintf(buf, "128000");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_TAIL);
    sprintf(buf, "-100");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_HEAD);
    sprintf(buf, "4294967296");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_HEAD);
    sprintf(buf, "non integer");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_TAIL);
    sprintf(buf, "much much longer non integer");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_TAIL);
    return zl;
}

long long usec(void) {
    struct timeval tv;
    gettimeofday(&tv,NULL);
    return (((long long)tv.tv_sec)*1000000)+tv.tv_usec;
}

void stress(int pos, int num, int maxsize, int dnum) {
    int i,j,k;
    unsigned char *zl;
    char posstr[2][5] = { "HEAD", "TAIL" };
    long long start;
    for (i = 0; i < maxsize; i+=dnum) {
        zl = ziplistNew();
        for (j = 0; j < i; j++) {
            zl = ziplistPush(zl,(unsigned char*)"quux",4,ZIPLIST_TAIL);
        }

        /* Do num times a push+pop from pos */
        start = usec();
        for (k = 0; k < num; k++) {
            zl = ziplistPush(zl,(unsigned char*)"quux",4,pos);
            zl = ziplistDeleteRange(zl,0,1);
        }
        printf("List size: %8d, bytes: %8d, %dx push+pop (%s): %6lld usec\n",
            i,intrev32ifbe(ZIPLIST_BYTES(zl)),num,posstr[pos],usec()-start);
        zfree(zl);
    }
}

void pop(unsigned char *zl, int where) {
    unsigned char *p, *vstr;
    unsigned int vlen;
    long long vlong;

    p = ziplistIndex(zl,where == ZIPLIST_HEAD ? 0 : -1);
    if (ziplistGet(p,&vstr,&vlen,&vlong)) {
        if (where == ZIPLIST_HEAD)
            printf("Pop head: ");
        else
            printf("Pop tail: ");

        if (vstr)
            if (vlen && fwrite(vstr,vlen,1,stdout) == 0) perror("fwrite");
        else
            printf("%lld", vlong);

        printf("\n");
        ziplistDeleteRange(zl,-1,1);
    } else {
        printf("ERROR: Could not pop\n");
        exit(1);
    }
}

int randstring(char *target, unsigned int min, unsigned int max) {
    int p = 0;
    int len = min+rand()%(max-min+1);
    int minval, maxval;
    switch(rand() % 3) {
    case 0:
        minval = 0;
        maxval = 255;
    break;
    case 1:
        minval = 48;
        maxval = 122;
    break;
    case 2:
        minval = 48;
        maxval = 52;
    break;
    default:
        assert(NULL);
    }

    while(p < len)
        target[p++] = minval+rand()%(maxval-minval+1);
    return len;
}

void verify(unsigned char *zl, zlentry *e) {
    int i;
    int len = ziplistLen(zl);
    zlentry _e;

    for (i = 0; i < len; i++) {
        memset(&e[i], 0, sizeof(zlentry));
        e[i] = zipEntry(ziplistIndex(zl, i));

        memset(&_e, 0, sizeof(zlentry));
        _e = zipEntry(ziplistIndex(zl, -len+i));

        assert(memcmp(&e[i], &_e, sizeof(zlentry)) == 0);
    }
}

int main(int argc, char **argv) {
    unsigned char *zl, *p;
    unsigned char *entry;
    unsigned int elen;
    long long value;

    /* If an argument is given, use it as the random seed. */
    if (argc == 2)
        srand(atoi(argv[1]));

    zl = createIntList();
    ziplistRepr(zl);

    zl = createList();
    ziplistRepr(zl);

    pop(zl,ZIPLIST_TAIL);
    ziplistRepr(zl);

    pop(zl,ZIPLIST_HEAD);
    ziplistRepr(zl);

    pop(zl,ZIPLIST_TAIL);
    ziplistRepr(zl);

    pop(zl,ZIPLIST_TAIL);
    ziplistRepr(zl);

    printf("Get element at index 3:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 3);
        if (!ziplistGet(p, &entry, &elen, &value)) {
            printf("ERROR: Could not access index 3\n");
            return 1;
        }
        if (entry) {
            if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            printf("\n");
        } else {
            printf("%lld\n", value);
        }
        printf("\n");
    }

    printf("Get element at index 4 (out of range):\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 4);
        if (p == NULL) {
            printf("No entry\n");
        } else {
            printf("ERROR: Out of range index should return NULL, returned offset: %ld\n", p-zl);
            return 1;
        }
        printf("\n");
    }

    printf("Get element at index -1 (last element):\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -1);
        if (!ziplistGet(p, &entry, &elen, &value)) {
            printf("ERROR: Could not access index -1\n");
            return 1;
        }
        if (entry) {
            if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            printf("\n");
        } else {
            printf("%lld\n", value);
        }
        printf("\n");
    }

    printf("Get element at index -4 (first element):\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -4);
        if (!ziplistGet(p, &entry, &elen, &value)) {
            printf("ERROR: Could not access index -4\n");
            return 1;
        }
        if (entry) {
            if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            printf("\n");
        } else {
            printf("%lld\n", value);
        }
        printf("\n");
    }

    printf("Get element at index -5 (reverse out of range):\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -5);
        if (p == NULL) {
            printf("No entry\n");
        } else {
            printf("ERROR: Out of range index should return NULL, returned offset: %ld\n", p-zl);
            return 1;
        }
        printf("\n");
    }

    printf("Iterate list from 0 to end:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 0);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            p = ziplistNext(zl,p);
            printf("\n");
        }
        printf("\n");
    }

    printf("Iterate list from 1 to end:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 1);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            p = ziplistNext(zl,p);
            printf("\n");
        }
        printf("\n");
    }

    printf("Iterate list from 2 to end:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 2);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            p = ziplistNext(zl,p);
            printf("\n");
        }
        printf("\n");
    }

    printf("Iterate starting out of range:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 4);
        if (!ziplistGet(p, &entry, &elen, &value)) {
            printf("No entry\n");
        } else {
            printf("ERROR\n");
        }
        printf("\n");
    }

    printf("Iterate from back to front:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -1);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            p = ziplistPrev(zl,p);
            printf("\n");
        }
        printf("\n");
    }

    printf("Iterate from back to front, deleting all items:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -1);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            zl = ziplistDelete(zl,&p);
            p = ziplistPrev(zl,p);
            printf("\n");
        }
        printf("\n");
    }

    printf("Delete inclusive range 0,0:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 0, 1);
        ziplistRepr(zl);
    }

    printf("Delete inclusive range 0,1:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 0, 2);
        ziplistRepr(zl);
    }

    printf("Delete inclusive range 1,2:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 1, 2);
        ziplistRepr(zl);
    }

    printf("Delete with start index out of range:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 5, 1);
        ziplistRepr(zl);
    }

    printf("Delete with num overflow:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 1, 5);
        ziplistRepr(zl);
    }

    printf("Delete foo while iterating:\n");
    {
        zl = createList();
        p = ziplistIndex(zl,0);
        while (ziplistGet(p,&entry,&elen,&value)) {
            if (entry && strncmp("foo",(char*)entry,elen) == 0) {
                printf("Delete foo\n");
                zl = ziplistDelete(zl,&p);
            } else {
                printf("Entry: ");
                if (entry) {
                    if (elen && fwrite(entry,elen,1,stdout) == 0)
                        perror("fwrite");
                } else {
                    printf("%lld",value);
                }
                p = ziplistNext(zl,p);
                printf("\n");
            }
        }
        printf("\n");
        ziplistRepr(zl);
    }

    printf("Regression test for >255 byte strings:\n");
    {
        char v1[257],v2[257];
        memset(v1,'x',256);
        memset(v2,'y',256);
        zl = ziplistNew();
        zl = ziplistPush(zl,(unsigned char*)v1,strlen(v1),ZIPLIST_TAIL);
        zl = ziplistPush(zl,(unsigned char*)v2,strlen(v2),ZIPLIST_TAIL);

        /* Pop values again and compare their value. */
        p = ziplistIndex(zl,0);
        assert(ziplistGet(p,&entry,&elen,&value));
        assert(strncmp(v1,(char*)entry,elen) == 0);
        p = ziplistIndex(zl,1);
        assert(ziplistGet(p,&entry,&elen,&value));
        assert(strncmp(v2,(char*)entry,elen) == 0);
        printf("SUCCESS\n\n");
    }

    printf("Regression test deleting next to last entries:\n");
    {
        char v[3][257];
        zlentry e[3];
        int i;

        for (i = 0; i < (sizeof(v)/sizeof(v[0])); i++) {
            memset(v[i], 'a' + i, sizeof(v[0]));
        }

        v[0][256] = '\0';
        v[1][  1] = '\0';
        v[2][256] = '\0';

        zl = ziplistNew();
        for (i = 0; i < (sizeof(v)/sizeof(v[0])); i++) {
            zl = ziplistPush(zl, (unsigned char *) v[i], strlen(v[i]), ZIPLIST_TAIL);
        }

        verify(zl, e);

        assert(e[0].prevrawlensize == 1);
        assert(e[1].prevrawlensize == 5);
        assert(e[2].prevrawlensize == 1);

        /* Deleting entry 1 will increase `prevrawlensize` for entry 2 */
        unsigned char *p = e[1].p;
        zl = ziplistDelete(zl, &p);

        verify(zl, e);

        assert(e[0].prevrawlensize == 1);
        assert(e[1].prevrawlensize == 5);

        printf("SUCCESS\n\n");
    }

    printf("Create long list and check indices:\n");
    {
        zl = ziplistNew();
        char buf[32];
        int i,len;
        for (i = 0; i < 1000; i++) {
            len = sprintf(buf,"%d",i);
            zl = ziplistPush(zl,(unsigned char*)buf,len,ZIPLIST_TAIL);
        }
        for (i = 0; i < 1000; i++) {
            p = ziplistIndex(zl,i);
            assert(ziplistGet(p,NULL,NULL,&value));
            assert(i == value);

            p = ziplistIndex(zl,-i-1);
            assert(ziplistGet(p,NULL,NULL,&value));
            assert(999-i == value);
        }
        printf("SUCCESS\n\n");
    }

    printf("Compare strings with ziplist entries:\n");
    {
        zl = createList();
        p = ziplistIndex(zl,0);
        if (!ziplistCompare(p,(unsigned char*)"hello",5)) {
            printf("ERROR: not \"hello\"\n");
            return 1;
        }
        if (ziplistCompare(p,(unsigned char*)"hella",5)) {
            printf("ERROR: \"hella\"\n");
            return 1;
        }

        p = ziplistIndex(zl,3);
        if (!ziplistCompare(p,(unsigned char*)"1024",4)) {
            printf("ERROR: not \"1024\"\n");
            return 1;
        }
        if (ziplistCompare(p,(unsigned char*)"1025",4)) {
            printf("ERROR: \"1025\"\n");
            return 1;
        }
        printf("SUCCESS\n\n");
    }

    printf("Stress with random payloads of different encoding:\n");
    {
        int i,j,len,where;
        unsigned char *p;
        char buf[1024];
        int buflen;
        list *ref;
        listNode *refnode;

        /* Hold temp vars from ziplist */
        unsigned char *sstr;
        unsigned int slen;
        long long sval;

        for (i = 0; i < 20000; i++) {
            zl = ziplistNew();
            ref = listCreate();
            listSetFreeMethod(ref,sdsfree);
            len = rand() % 256;

            /* Create lists */
            for (j = 0; j < len; j++) {
                where = (rand() & 1) ? ZIPLIST_HEAD : ZIPLIST_TAIL;
                if (rand() % 2) {
                    buflen = randstring(buf,1,sizeof(buf)-1);
                } else {
                    switch(rand() % 3) {
                    case 0:
                        buflen = sprintf(buf,"%lld",(0LL + rand()) >> 20);
                        break;
                    case 1:
                        buflen = sprintf(buf,"%lld",(0LL + rand()));
                        break;
                    case 2:
                        buflen = sprintf(buf,"%lld",(0LL + rand()) << 20);
                        break;
                    default:
                        assert(NULL);
                    }
                }

                /* Add to ziplist */
                zl = ziplistPush(zl, (unsigned char*)buf, buflen, where);

                /* Add to reference list */
                if (where == ZIPLIST_HEAD) {
                    listAddNodeHead(ref,sdsnewlen(buf, buflen));
                } else if (where == ZIPLIST_TAIL) {
                    listAddNodeTail(ref,sdsnewlen(buf, buflen));
                } else {
                    assert(NULL);
                }
            }

            assert(listLength(ref) == ziplistLen(zl));
            for (j = 0; j < len; j++) {
                /* Naive way to get elements, but similar to the stresser
                 * executed from the Tcl test suite. */
                p = ziplistIndex(zl,j);
                refnode = listIndex(ref,j);

                assert(ziplistGet(p,&sstr,&slen,&sval));
                if (sstr == NULL) {
                    buflen = sprintf(buf,"%lld",sval);
                } else {
                    buflen = slen;
                    memcpy(buf,sstr,buflen);
                    buf[buflen] = '\0';
                }
                assert(memcmp(buf,listNodeValue(refnode),buflen) == 0);
            }
            zfree(zl);
            listRelease(ref);
        }
        printf("SUCCESS\n\n");
    }

    printf("Stress with variable ziplist size:\n");
    {
        stress(ZIPLIST_HEAD,100000,16384,256);
        stress(ZIPLIST_TAIL,100000,16384,256);
    }

    return 0;
}

#endif
