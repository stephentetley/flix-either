/*
 * Copyright 2026 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


mod Choice3 {

    ///
    /// The Choice3 type.
    ///
    pub enum Choice3[a, b, c] with Eq, Order, ToString {
        case A(a)
        case B(b)
        case C(c)
    }


    ///
    /// Returns `true` iff `x` is `A(_)`.
    ///
    pub def isA(x: Choice3[a, b, c]): Bool = match x {
        case Choice3.A(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `B(_)`.
    ///
    pub def isB(x: Choice3[a, b, c]): Bool = match x {
        case Choice3.B(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `C(_)`.
    ///
    pub def isC(x: Choice3[a, b, c]): Bool = match x {
        case Choice3.C(_)   => true
        case _              => false
    }

    pub def getWithDefaultA(d: a, x: Choice3[a, b, c]): a = match x {
        case Choice3.A(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultB(d: b, x: Choice3[a, b, c]): b = match x {
        case Choice3.B(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultC(d: c, x: Choice3[a, b, c]): c = match x {
        case Choice3.C(x1)  => x1
        case _              => d
    }

    // Wrong name?
    pub def map3(f1: a -> d \ ef1, 
                    f2: b -> d \ ef2, 
                    f3: c -> d \ ef3, 
                    x: Choice3[a, b, c]): d \ {ef1, ef2, ef3} = match x {
        case Choice3.A(a)   => checked_ecast(f1(a))
        case Choice3.B(b)   => checked_ecast(f2(b))
        case Choice3.C(c)   => checked_ecast(f3(c))
    }

    pub def mapA(fn: a -> d \ ef, x: Choice3[a, b, c]): Choice3[d, b, c] \ ef = match x {
        case Choice3.A(a)   => Choice3.A(fn(a))
        case Choice3.B(b)   => checked_ecast(Choice3.B(b))
        case Choice3.C(c)   => checked_ecast(Choice3.C(c))
    }

    pub def mapB(fn: b -> d \ ef, x: Choice3[a, b, c]): Choice3[a, d, c] \ ef = match x {
        case Choice3.A(a)   => checked_ecast(Choice3.A(a))
        case Choice3.B(b)   => Choice3.B(fn(b))
        case Choice3.C(c)   => checked_ecast(Choice3.C(c))
    }

    pub def mapC(fn: c -> d \ ef, x: Choice3[a, b, c]): Choice3[a, b, d] \ ef = match x {
        case Choice3.A(a)   => checked_ecast(Choice3.A(a))
        case Choice3.B(b)   => checked_ecast(Choice3.B(b))
        case Choice3.C(c)   => Choice3.C(fn(c))
    }



    pub def partition(l: List[Choice3[a, b, c]]): (List[a], List[b], List[c]) =
        def loop(xs, ka, kb, kc) = match xs {
            case Nil     => (ka(Nil), kb(Nil), kc(Nil))
            case x :: rs => match x {
                case Choice3.A(a)   => loop(rs, ks -> ka(a :: ks), kb, kc)
                case Choice3.B(b)   => loop(rs, ka, ks -> kb(b :: ks), kc)
                case Choice3.C(c)   => loop(rs, ka, kb, ks -> kc(c :: ks))
            }
        };
        loop(l, identity, identity, identity)


    pub def allA(l: List[Choice3[a, b, c]]): List[a] =
        def loop(xs, ka) = match xs {
            case Nil     => ka(Nil)
            case x :: rs => match x {
                case Choice3.A(a)   => loop(rs, ks -> ka(a :: ks))
                case _              => loop(rs, ka)
            }
        };
        loop(l, identity)

    pub def allB(l: List[Choice3[a, b, c]]): List[b] =
        def loop(xs, kb) = match xs {
            case Nil     => kb(Nil)
            case x :: rs => match x {
                case Choice3.B(b)   => loop(rs, ks -> kb(b :: ks))
                case _              => loop(rs, kb)
            }
        };
        loop(l, identity)

    pub def allC(l: List[Choice3[a, b, c]]): List[c] =
        def loop(xs, kc) = match xs {
            case Nil     => kc(Nil)
            case x :: rs => match x {
                case Choice3.C(c)   => loop(rs, ks -> kc(c :: ks))
                case _              => loop(rs, kc)
            }
        };
        loop(l, identity)

}

