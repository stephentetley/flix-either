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


mod Choice5 {

    ///
    /// The Choice5 type.
    ///
    pub enum Choice5[a, b, c, d, e] with Eq, Order, ToString {
        case A(a)
        case B(b)
        case C(c)
        case D(d)
        case E(e)
    }


    ///
    /// Returns `true` iff `x` is `A(_)`.
    ///
    pub def isA(x: Choice5[a, b, c, d, e]): Bool = match x {
        case Choice5.A(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `B(_)`.
    ///
    pub def isB(x: Choice5[a, b, c, d, e]): Bool = match x {
        case Choice5.B(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `C(_)`.
    ///
    pub def isC(x: Choice5[a, b, c, d, e]): Bool = match x {
        case Choice5.C(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `D(_)`.
    ///
    pub def isD(x: Choice5[a, b, c, d, e]): Bool = match x {
        case Choice5.D(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `E(_)`.
    ///
    pub def isE(x: Choice5[a, b, c, d, e]): Bool = match x {
        case Choice5.E(_)   => true
        case _              => false
    }


    pub def getWithDefaultA(d: a, x: Choice5[a, b, c, d, e]): a = match x {
        case Choice5.A(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultB(d: b, x: Choice5[a, b, c, d, e]): b = match x {
        case Choice5.B(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultC(d: c, x: Choice5[a, b, c, d, e]): c = match x {
        case Choice5.C(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultD(d: d, x: Choice5[a, b, c, d, e]): d = match x {
        case Choice5.D(x1)  => x1
        case _              => d 
    }

    pub def getWithDefaultE(d: e, x: Choice5[a, b, c, d, e]): e = match x {
        case Choice5.E(x1)  => x1
        case _              => d 
    }

    // Wrong name?
    pub def map5(f1: a -> f \ ef1, 
                    f2: b -> f \ ef2, 
                    f3: c -> f \ ef3, 
                    f4: d -> f \ ef4, 
                    f5: e -> f \ ef5,
                    x: Choice5[a, b, c, d, e]): f \ {ef1, ef2, ef3, ef4, ef5} = match x {
        case Choice5.A(a)   => checked_ecast(f1(a))
        case Choice5.B(b)   => checked_ecast(f2(b))
        case Choice5.C(c)   => checked_ecast(f3(c))
        case Choice5.D(d)   => checked_ecast(f4(d))
        case Choice5.E(e)   => checked_ecast(f5(e))
    }

    pub def mapA(fn: a -> f \ ef, x: Choice5[a, b, c, d, e]): Choice5[f, b, c, d, e] \ ef = match x {
        case Choice5.A(a)   => Choice5.A(fn(a))
        case Choice5.B(b)   => checked_ecast(Choice5.B(b))
        case Choice5.C(c)   => checked_ecast(Choice5.C(c))
        case Choice5.D(d)   => checked_ecast(Choice5.D(d))
        case Choice5.E(e)   => checked_ecast(Choice5.E(e))
    }

    pub def mapB(fn: b -> f \ ef, x: Choice5[a, b, c, d, e]): Choice5[a, f, c, d, e] \ ef = match x {
        case Choice5.A(a)   => checked_ecast(Choice5.A(a))
        case Choice5.B(b)   => Choice5.B(fn(b))
        case Choice5.C(c)   => checked_ecast(Choice5.C(c))
        case Choice5.D(d)   => checked_ecast(Choice5.D(d))
        case Choice5.E(e)   => checked_ecast(Choice5.E(e))
    }

    pub def mapC(fn: c -> f \ ef, x: Choice5[a, b, c, d, e]): Choice5[a, b, f, d, e] \ ef = match x {
        case Choice5.A(a)   => checked_ecast(Choice5.A(a))
        case Choice5.B(b)   => checked_ecast(Choice5.B(b))
        case Choice5.C(c)   => Choice5.C(fn(c))
        case Choice5.D(d)   => checked_ecast(Choice5.D(d))
        case Choice5.E(e)   => checked_ecast(Choice5.E(e))
    }

    pub def mapD(fn: d -> f \ ef, x: Choice5[a, b, c, d, e]): Choice5[a, b, c, f, e] \ ef = match x {
        case Choice5.A(a)   => checked_ecast(Choice5.A(a))
        case Choice5.B(b)   => checked_ecast(Choice5.B(b))
        case Choice5.C(c)   => checked_ecast(Choice5.C(c))
        case Choice5.D(d)   => Choice5.D(fn(d))
        case Choice5.E(e)   => checked_ecast(Choice5.E(e))
    }

    pub def mapE(fn: e -> f \ ef, x: Choice5[a, b, c, d, e]): Choice5[a, b, c, d, f] \ ef = match x {
        case Choice5.A(a)   => checked_ecast(Choice5.A(a))
        case Choice5.B(b)   => checked_ecast(Choice5.B(b))
        case Choice5.C(c)   => checked_ecast(Choice5.C(c))
        case Choice5.D(d)   => checked_ecast(Choice5.D(d))
        case Choice5.E(e)   => Choice5.E(fn(e))
    }


    pub def partition(l: List[Choice5[a, b, c, d, e]]): (List[a], List[b], List[c], List[d], List[e]) =
        def loop(xs, ka, kb, kc, kd, ke) = match xs {
            case Nil     => (ka(Nil), kb(Nil), kc(Nil), kd(Nil), ke(Nil))
            case x :: rs => match x {
                case Choice5.A(a)   => loop(rs, ks -> ka(a :: ks), kb, kc, kd, ke)
                case Choice5.B(b)   => loop(rs, ka, ks -> kb(b :: ks), kc, kd, ke)
                case Choice5.C(c)   => loop(rs, ka, kb, ks -> kc(c :: ks), kd, ke)
                case Choice5.D(d)   => loop(rs, ka, kb, kc, ks -> kd(d :: ks), ke)
                case Choice5.E(e)   => loop(rs, ka, kb, kc, kd, ks -> ke(e :: ks))
            }
        };
        loop(l, identity, identity, identity, identity, identity)


    pub def allA(l: List[Choice5[a, b, c, d, e]]): List[a] =
        def loop(xs, ka) = match xs {
            case Nil     => ka(Nil)
            case x :: rs => match x {
                case Choice5.A(a)   => loop(rs, ks -> ka(a :: ks))
                case _              => loop(rs, ka)
            }
        };
        loop(l, identity)

    pub def allB(l: List[Choice5[a, b, c, d, e]]): List[b] =
        def loop(xs, kb) = match xs {
            case Nil     => kb(Nil)
            case x :: rs => match x {
                case Choice5.B(b)   => loop(rs, ks -> kb(b :: ks))
                case _              => loop(rs, kb)
            }
        };
        loop(l, identity)

    pub def allC(l: List[Choice5[a, b, c, d, e]]): List[c] =
        def loop(xs, kc) = match xs {
            case Nil     => kc(Nil)
            case x :: rs => match x {
                case Choice5.C(c)   => loop(rs, ks -> kc(c :: ks))
                case _              => loop(rs, kc)
            }
        };
        loop(l, identity)

    pub def allD(l: List[Choice5[a, b, c, d, e]]): List[d] =
        def loop(xs, kd) = match xs {
            case Nil     => kd(Nil)
            case x :: rs => match x {
                case Choice5.D(d)   => loop(rs, ks -> kd(d :: ks))
                case _              => loop(rs, kd)
            }
        };
        loop(l, identity)

    pub def allE(l: List[Choice5[a, b, c, d, e]]): List[e] =
        def loop(xs, ke) = match xs {
            case Nil     => ke(Nil)
            case x :: rs => match x {
                case Choice5.E(e)   => loop(rs, ks -> ke(e :: ks))
                case _              => loop(rs, ke)
            }
        };
        loop(l, identity)

}

