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


mod Choice4 {

    ///
    /// The Choice4 type.
    ///
    pub enum Choice4[a, b, c, d] with Eq, Order, ToString {
        case A(a)
        case B(b)
        case C(c)
        case D(d)
    }


    ///
    /// Returns `true` iff `x` is `A(_)`.
    ///
    pub def isA(e: Choice4[a, b, c, d]): Bool = match e {
        case Choice4.A(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `e` is `B(_)`.
    ///
    pub def isB(e: Choice4[a, b, c, d]): Bool = match e {
        case Choice4.B(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `e` is `C(_)`.
    ///
    pub def isC(e: Choice4[a, b, c, d]): Bool = match e {
        case Choice4.C(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `e` is `D(_)`.
    ///
    pub def isD(e: Choice4[a, b, c, d]): Bool = match e {
        case Choice4.D(_)   => true
        case _              => false
    }

    pub def getWithDefaultA(d: a, x: Choice4[a, b, c, d]): a = match x {
        case Choice4.A(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultB(d: b, x: Choice4[a, b, c, d]): b = match x {
        case Choice4.B(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultC(d: c, x: Choice4[a, b, c, d]): c = match x {
        case Choice4.C(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultD(d: d, x: Choice4[a, b, c, d]): d = match x {
        case Choice4.D(x1)  => x1
        case _              => d
    }

    // Wrong name?
    pub def map4(f1: a -> e \ ef1, 
                    f2: b -> e \ ef2, 
                    f3: c -> e \ ef3, 
                    f4: d -> e \ ef4, 
                    x: Choice4[a, b, c, d]): e \ {ef1, ef2, ef3, ef4} = match x {
        case Choice4.A(a)   => checked_ecast(f1(a))
        case Choice4.B(b)   => checked_ecast(f2(b))
        case Choice4.C(c)   => checked_ecast(f3(c))
        case Choice4.D(d)   => checked_ecast(f4(d))
    }

    pub def mapA(f: a -> e \ ef, x: Choice4[a, b, c, d]): Choice4[e, b, c, d] \ ef = match x {
        case Choice4.A(a)   => Choice4.A(f(a))
        case Choice4.B(b)   => checked_ecast(Choice4.B(b))
        case Choice4.C(c)   => checked_ecast(Choice4.C(c))
        case Choice4.D(d)   => checked_ecast(Choice4.D(d))
    }

    pub def mapB(f: b -> e \ ef, x: Choice4[a, b, c, d]): Choice4[a, e, c, d] \ ef = match x {
        case Choice4.A(a)   => checked_ecast(Choice4.A(a))
        case Choice4.B(b)   => Choice4.B(f(b))
        case Choice4.C(c)   => checked_ecast(Choice4.C(c))
        case Choice4.D(d)   => checked_ecast(Choice4.D(d))
    }

    pub def mapC(f: c -> e \ ef, x: Choice4[a, b, c, d]): Choice4[a, b, e, d] \ ef = match x {
        case Choice4.A(a)   => checked_ecast(Choice4.A(a))
        case Choice4.B(b)   => checked_ecast(Choice4.B(b))
        case Choice4.C(c)   => Choice4.C(f(c))
        case Choice4.D(d)   => checked_ecast(Choice4.D(d))
    }

    pub def mapD(f: d -> e \ ef, x: Choice4[a, b, c, d]): Choice4[a, b, c, e] \ ef = match x {
        case Choice4.A(a)   => checked_ecast(Choice4.A(a))
        case Choice4.B(b)   => checked_ecast(Choice4.B(b))
        case Choice4.C(c)   => checked_ecast(Choice4.C(c))
        case Choice4.D(d)   => Choice4.D(f(d))
    }



    pub def partition(l: List[Choice4[a, b, c, d]]): (List[a], List[b], List[c], List[d]) =
        def loop(xs, ka, kb, kc, kd) = match xs {
            case Nil     => (ka(Nil), kb(Nil), kc(Nil), kd(Nil))
            case x :: rs => match x {
                case Choice4.A(a)   => loop(rs, ks -> ka(a :: ks), kb, kc, kd)
                case Choice4.B(b)   => loop(rs, ka, ks -> kb(b :: ks), kc, kd)
                case Choice4.C(c)   => loop(rs, ka, kb, ks -> kc(c :: ks), kd)
                case Choice4.D(d)   => loop(rs, ka, kb, kc, ks -> kd(d :: ks))
            }
        };
        loop(l, identity, identity, identity, identity)


    pub def allA(l: List[Choice4[a, b, c, d]]): List[a] =
        def loop(xs, ka) = match xs {
            case Nil     => ka(Nil)
            case x :: rs => match x {
                case Choice4.A(a)   => loop(rs, ks -> ka(a :: ks))
                case _              => loop(rs, ka)
            }
        };
        loop(l, identity)

    pub def allB(l: List[Choice4[a, b, c, d]]): List[b] =
        def loop(xs, kb) = match xs {
            case Nil     => kb(Nil)
            case x :: rs => match x {
                case Choice4.B(b)   => loop(rs, ks -> kb(b :: ks))
                case _              => loop(rs, kb)
            }
        };
        loop(l, identity)

    pub def allC(l: List[Choice4[a, b, c, d]]): List[c] =
        def loop(xs, kc) = match xs {
            case Nil     => kc(Nil)
            case x :: rs => match x {
                case Choice4.C(c)   => loop(rs, ks -> kc(c :: ks))
                case _              => loop(rs, kc)
            }
        };
        loop(l, identity)

    pub def allD(l: List[Choice4[a, b, c, d]]): List[d] =
        def loop(xs, kd) = match xs {
            case Nil     => kd(Nil)
            case x :: rs => match x {
                case Choice4.D(d)   => loop(rs, ks -> kd(d :: ks))
                case _              => loop(rs, kd)
            }
        };
        loop(l, identity)
}

