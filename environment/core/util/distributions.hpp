//===----------------------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See below for details.
//
//===----------------------------------------------------------------------===//

// Derived from Clang libcxx 7e73ea8.

// ==============================================================================
// libc++ License
// ==============================================================================

// The libc++ library is dual licensed under both the University of Illinois
// "BSD-Like" license and the MIT license.  As a user of this code you may choose
// to use it under either license.  As a contributor, you agree to allow your code
// to be used under both.

// Full text of the relevant licenses is included below.

// ==============================================================================

// University of Illinois/NCSA
// Open Source License

// Copyright (c) 2009-2017 by the contributors listed in CREDITS.TXT

// All rights reserved.

// Developed by:

//     LLVM Team

//     University of Illinois at Urbana-Champaign

//     http://llvm.org

// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal with
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do
// so, subject to the following conditions:

//     * Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimers.

//     * Redistributions in binary form must reproduce the above copyright notice,
//       this list of conditions and the following disclaimers in the
//       documentation and/or other materials provided with the distribution.

//     * Neither the names of the LLVM Team, University of Illinois at
//       Urbana-Champaign, nor the names of its contributors may be used to
//       endorse or promote products derived from this Software without specific
//       prior written permission.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
// CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
// SOFTWARE.

// ==============================================================================

// Copyright (c) 2009-2014 by the contributors listed in CREDITS.TXT

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#include <cstdint>
#include <limits>
#include <type_traits>

#ifdef _LIBCPP_COMPILER_MSVC
#include <intrin.h>
#endif

#ifndef __CHAR_BIT__
#define __CHAR_BIT__ CHAR_BIT
#endif

namespace util {
    // Precondition:  __x != 0
    static unsigned __clz(unsigned __x) {
#ifndef _LIBCPP_COMPILER_MSVC
        return static_cast<unsigned>(__builtin_clz(__x));
#else
        static_assert(sizeof(unsigned) == sizeof(unsigned long), "");
        static_assert(sizeof(unsigned long) == 4, "");
        unsigned long where = 0;
        // Search from LSB to MSB for first set bit.
        // Returns zero if no set bit is found.
        if (_BitScanReverse(&where, __x))
            return 31 - where;
        return 32; // Undefined Behavior.
#endif
    }

    static unsigned long __clz(unsigned long __x) {
#ifndef _LIBCPP_COMPILER_MSVC
        return static_cast<unsigned long>(__builtin_clzl (__x));
#else
        static_assert(sizeof(unsigned) == sizeof(unsigned long), "");
        return __clz(static_cast<unsigned>(__x));
#endif
    }

    static unsigned long long __clz(unsigned long long __x) {
#ifndef _LIBCPP_COMPILER_MSVC
        return static_cast<unsigned long long>(__builtin_clzll(__x));
#else
        unsigned long where = 0;
        // BitScanReverse scans from MSB to LSB for first set bit.
        // Returns 0 if no set bit is found.
#if defined(_LIBCPP_HAS_BITSCAN64)
        if (_BitScanReverse64(&where, __x))
            return static_cast<int>(63 - where);
#else
        // Scan the high 32 bits.
        if (_BitScanReverse(&where, static_cast<unsigned long>(__x >> 32)))
            return 63 - (where + 32); // Create a bit offset from the MSB.
        // Scan the low 32 bits.
        if (_BitScanReverse(&where, static_cast<unsigned long>(__x)))
            return 63 - where;
#endif
        return 64; // Undefined Behavior.
#endif // _LIBCPP_COMPILER_MSVC
    }

    template <unsigned long long _Xp, std::size_t _Rp>
    struct __log2_imp
    {
        static const std::size_t value = _Xp & ((unsigned long long)(1) << _Rp) ? _Rp
                                                                           : __log2_imp<_Xp, _Rp - 1>::value;
    };

    template <unsigned long long _Xp>
    struct __log2_imp<_Xp, 0>
    {
        static const std::size_t value = 0;
    };

    template <std::size_t _Rp>
    struct __log2_imp<0, _Rp>
    {
        static const std::size_t value = _Rp + 1;
    };

    template <class _UIntType, _UIntType _Xp>
    struct __log2
    {
        static const std::size_t value = __log2_imp<_Xp,
                                                    sizeof(_UIntType) * __CHAR_BIT__ - 1>::value;
    };

    template<class _Engine, class _UIntType>
    class independent_bits_engine
    {
    public:
        // types
        typedef _UIntType result_type;

    private:
        typedef typename _Engine::result_type _Engine_result_type;
        typedef typename std::conditional
        <
            sizeof(_Engine_result_type) <= sizeof(result_type),
                                           result_type,
                                           _Engine_result_type
                                           >::type _Working_result_type;

        _Engine& __e_;
        std::size_t __w_;
        std::size_t __w0_;
        std::size_t __n_;
        std::size_t __n0_;
        _Working_result_type __y0_;
        _Working_result_type __y1_;
        _Engine_result_type __mask0_;
        _Engine_result_type __mask1_;

#ifdef _LIBCPP_COMPILER_MSVC
#pragma warning(push)
#pragma warning(disable: 4307) // integral constant overflow
#endif
        static constexpr const _Working_result_type _Rp = _Engine::max() - _Engine::min()
            + _Working_result_type(1);
#ifdef _LIBCPP_COMPILER_MSVC
#pragma warning(pop)
#endif

        static constexpr const std::size_t __m = __log2<_Working_result_type, _Rp>::value;
        static constexpr const std::size_t _WDt = std::numeric_limits<_Working_result_type>::digits;
        static constexpr const std::size_t _EDt = std::numeric_limits<_Engine_result_type>::digits;

    public:
        // constructors and seeding functions
        independent_bits_engine(_Engine& __e, std::size_t __w);

        // generating functions
        result_type operator()() {return __eval(std::integral_constant<bool, _Rp != 0>());}

    private:
        result_type __eval(std::false_type);
        result_type __eval(std::true_type);
    };

    template<class _Engine, class _UIntType>
    independent_bits_engine<_Engine, _UIntType>
    ::independent_bits_engine(_Engine& __e, std::size_t __w)
        : __e_(__e),
          __w_(__w)
    {
        __n_ = __w_ / __m + (__w_ % __m != 0);
        __w0_ = __w_ / __n_;
        if (_Rp == 0)
            __y0_ = _Rp;
        else if (__w0_ < _WDt)
            __y0_ = (_Rp >> __w0_) << __w0_;
        else
            __y0_ = 0;
        if (_Rp - __y0_ > __y0_ / __n_)
            {
                ++__n_;
                __w0_ = __w_ / __n_;
                if (__w0_ < _WDt)
                    __y0_ = (_Rp >> __w0_) << __w0_;
                else
                    __y0_ = 0;
            }
        __n0_ = __n_ - __w_ % __n_;
        if (__w0_ < _WDt - 1)
            __y1_ = (_Rp >> (__w0_ + 1)) << (__w0_ + 1);
        else
            __y1_ = 0;
        __mask0_ = __w0_ > 0 ? _Engine_result_type(~0) >> (_EDt - __w0_) :
            _Engine_result_type(0);
        __mask1_ = __w0_ < _EDt - 1 ?
                           _Engine_result_type(~0) >> (_EDt - (__w0_ + 1)) :
            _Engine_result_type(~0);
    }

    template<class _Engine, class _UIntType>
    inline
    _UIntType
    independent_bits_engine<_Engine, _UIntType>::__eval(std::false_type)
    {
        return static_cast<result_type>(__e_() & __mask0_);
    }

    template<class _Engine, class _UIntType>
    _UIntType
    independent_bits_engine<_Engine, _UIntType>::__eval(std::true_type)
    {
        const std::size_t _WRt = std::numeric_limits<result_type>::digits;
        result_type _Sp = 0;
        for (std::size_t __k = 0; __k < __n0_; ++__k)
            {
                _Engine_result_type __u;
                do
                    {
                        __u = __e_() - _Engine::min();
                    } while (__u >= __y0_);
                if (__w0_ < _WRt)
                    _Sp <<= __w0_;
                else
                    _Sp = 0;
                _Sp += __u & __mask0_;
            }
        for (std::size_t __k = __n0_; __k < __n_; ++__k)
            {
                _Engine_result_type __u;
                do
                    {
                        __u = __e_() - _Engine::min();
                    } while (__u >= __y1_);
                if (__w0_ < _WRt - 1)
                    _Sp <<= __w0_ + 1;
                else
                    _Sp = 0;
                _Sp += __u & __mask1_;
            }
        return _Sp;
    }

    template<class _IntType = int>
    class uniform_int_distribution
    {
    public:
        // types
        typedef _IntType result_type;

        class param_type
        {
            result_type __a_;
            result_type __b_;
        public:
            typedef uniform_int_distribution distribution_type;

            explicit param_type(result_type __a = 0,
                                result_type __b = std::numeric_limits<result_type>::max())
                : __a_(__a), __b_(__b) {}

            result_type a() const {return __a_;}
            result_type b() const {return __b_;}

            friend bool operator==(const param_type& __x, const param_type& __y)
            {return __x.__a_ == __y.__a_ && __x.__b_ == __y.__b_;}
            friend bool operator!=(const param_type& __x, const param_type& __y)
            {return !(__x == __y);}
        };

    private:
        param_type __p_;

    public:
        // constructors and reset functions
        explicit uniform_int_distribution(result_type __a = 0,
                                          result_type __b = std::numeric_limits<result_type>::max())
            : __p_(param_type(__a, __b)) {}
        explicit uniform_int_distribution(const param_type& __p) : __p_(__p) {}
        void reset() {}

        // generating functions
        template<class _URNG> result_type operator()(_URNG& __g)
        {return (*this)(__g, __p_);}
        template<class _URNG> result_type operator()(_URNG& __g, const param_type& __p);

        // property functions
        result_type a() const {return __p_.a();}
        result_type b() const {return __p_.b();}

        param_type param() const {return __p_;}
        void param(const param_type& __p) {__p_ = __p;}

        result_type min() const {return a();}
        result_type max() const {return b();}

        friend bool operator==(const uniform_int_distribution& __x,
                               const uniform_int_distribution& __y)
        {return __x.__p_ == __y.__p_;}
        friend bool operator!=(const uniform_int_distribution& __x,
                               const uniform_int_distribution& __y)
        {return !(__x == __y);}
    };

    template<class _IntType>
    template<class _URNG>
    typename uniform_int_distribution<_IntType>::result_type
    uniform_int_distribution<_IntType>::operator()(_URNG& __g, const param_type& __p)
    {
        typedef typename std::conditional<sizeof(result_type) <= sizeof(std::uint32_t),
            uint32_t, uint64_t>::type _UIntType;
        const _UIntType _Rp = __p.b() - __p.a() + _UIntType(1);
        if (_Rp == 1)
            return __p.a();
        const std::size_t _Dt = std::numeric_limits<_UIntType>::digits;
        typedef independent_bits_engine<_URNG, _UIntType> _Eng;
        if (_Rp == 0)
            return static_cast<result_type>(_Eng(__g, _Dt)());
        std::size_t __w = _Dt - __clz(_Rp) - 1;
        if ((_Rp & (std::numeric_limits<_UIntType>::max() >> (_Dt - __w))) != 0)
            ++__w;
        _Eng __e(__g, __w);
        _UIntType __u;
        do
            {
                __u = __e();
            } while (__u >= _Rp);
        return static_cast<result_type>(__u + __p.a());
    }

    template<class _RealType, std::size_t __bits, class _URNG>
    _RealType
    generate_canonical(_URNG& __g)
    {
        const std::size_t _Dt = std::numeric_limits<_RealType>::digits;
        const std::size_t __b = _Dt < __bits ? _Dt : __bits;
        const std::size_t __logR = __log2<uint64_t, _URNG::max() - _URNG::min() + uint64_t(1)>::value;
        const std::size_t __k = __b / __logR + (__b % __logR != 0) + (__b == 0);
        const _RealType _Rp = _URNG::max() - _URNG::min() + _RealType(1);
        _RealType __base = _Rp;
        _RealType _Sp = __g() - _URNG::min();
        for (std::size_t __i = 1; __i < __k; ++__i, __base *= _Rp)
            _Sp += (__g() - _URNG::min()) * __base;
        return _Sp / __base;
    }

    template<class _RealType = double>
    class uniform_real_distribution
    {
    public:
        // types
        typedef _RealType result_type;

        class param_type
        {
            result_type __a_;
            result_type __b_;
        public:
            typedef uniform_real_distribution distribution_type;

            explicit param_type(result_type __a = 0,
                                result_type __b = 1)
                : __a_(__a), __b_(__b) {}

            result_type a() const {return __a_;}
            result_type b() const {return __b_;}

            friend
            bool operator==(const param_type& __x, const param_type& __y)
            {return __x.__a_ == __y.__a_ && __x.__b_ == __y.__b_;}
            friend
            bool operator!=(const param_type& __x, const param_type& __y)
            {return !(__x == __y);}
        };

    private:
        param_type __p_;

    public:
        // constructors and reset functions
        explicit uniform_real_distribution(result_type __a = 0, result_type __b = 1)
            : __p_(param_type(__a, __b)) {}
        explicit uniform_real_distribution(const param_type& __p) : __p_(__p) {}
        void reset() {}

        // generating functions
        template<class _URNG>
        result_type operator()(_URNG& __g)
        {return (*this)(__g, __p_);}
        template<class _URNG> result_type operator()(_URNG& __g, const param_type& __p);

        // property functions
        result_type a() const {return __p_.a();}
        result_type b() const {return __p_.b();}

        param_type param() const {return __p_;}

        void param(const param_type& __p) {__p_ = __p;}


        result_type min() const {return a();}

        result_type max() const {return b();}

        friend
        bool operator==(const uniform_real_distribution& __x,
                        const uniform_real_distribution& __y)
        {return __x.__p_ == __y.__p_;}
        friend
        bool operator!=(const uniform_real_distribution& __x,
                        const uniform_real_distribution& __y)
        {return !(__x == __y);}
    };

    template<class _RealType>
    template<class _URNG>
    inline
    typename uniform_real_distribution<_RealType>::result_type
    uniform_real_distribution<_RealType>::operator()(_URNG& __g, const param_type& __p)
    {
        return (__p.b() - __p.a())
            * ::util::generate_canonical<_RealType, std::numeric_limits<_RealType>::digits>(__g)
            + __p.a();
    }
}
