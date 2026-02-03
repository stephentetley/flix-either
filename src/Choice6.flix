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


mod Choice6 {

    ///
    /// The Choice6 type.
    ///
    pub enum Choice6[a, b, c, d, e, f] with Eq, Order, ToString {
        case A(a)
        case B(b)
        case C(c)
        case D(d)
        case E(e)
        case F(f)
    }


    ///
    /// Returns `true` iff `x` is `A(_)`.
    ///
    pub def isA(x: Choice6[a, b, c, d, e, f]): Bool = match x {
        case Choice6.A(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `B(_)`.
    ///
    pub def isB(x: Choice6[a, b, c, d, e, f]): Bool = match x {
        case Choice6.B(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `C(_)`.
    ///
    pub def isC(x: Choice6[a, b, c, d, e, f]): Bool = match x {
        case Choice6.C(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `D(_)`.
    ///
    pub def isD(x: Choice6[a, b, c, d, e, f]): Bool = match x {
        case Choice6.D(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `E(_)`.
    ///
    pub def isE(x: Choice6[a, b, c, d, e, f]): Bool = match x {
        case Choice6.E(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `F(_)`.
    ///
    pub def isF(x: Choice6[a, b, c, d, e, f]): Bool = match x {
        case Choice6.F(_)   => true
        case _              => false
    }


    pub def getWithDefaultA(d: a, x: Choice6[a, b, c, d, e, f]): a = match x {
        case Choice6.A(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultB(d: b, x: Choice6[a, b, c, d, e, f]): b = match x {
        case Choice6.B(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultC(d: c, x: Choice6[a, b, c, d, e, f]): c = match x {
        case Choice6.C(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultD(d: d, x: Choice6[a, b, c, d, e, f]): d = match x {
        case Choice6.D(x1)  => x1
        case _              => d 
    }

    pub def getWithDefaultE(d: e, x: Choice6[a, b, c, d, e, f]): e = match x {
        case Choice6.E(x1)  => x1
        case _              => d 
    }

    pub def getWithDefaultF(d: f, x: Choice6[a, b, c, d, e, f]): f = match x {
        case Choice6.F(x1)  => x1
        case _              => d 
    }

    // Wrong name?
    pub def map6(f1: a -> g \ ef1, 
                    f2: b -> g \ ef2, 
                    f3: c -> g \ ef3, 
                    f4: d -> g \ ef4, 
                    f5: e -> g \ ef5,
                    f6: f -> g \ ef6,
                    x: Choice6[a, b, c, d, e, f]): g \ {ef1, ef2, ef3, ef4, ef5, ef6} = match x {
        case Choice6.A(a)   => checked_ecast(f1(a))
        case Choice6.B(b)   => checked_ecast(f2(b))
        case Choice6.C(c)   => checked_ecast(f3(c))
        case Choice6.D(d)   => checked_ecast(f4(d))
        case Choice6.E(e)   => checked_ecast(f5(e))
        case Choice6.F(f)   => checked_ecast(f6(f))
    }

    pub def mapA(fn: a -> g \ ef, x: Choice6[a, b, c, d, e, f]): Choice6[g, b, c, d, e, f] \ ef = match x {
        case Choice6.A(a)   => Choice6.A(fn(a))
        case Choice6.B(b)   => checked_ecast(Choice6.B(b))
        case Choice6.C(c)   => checked_ecast(Choice6.C(c))
        case Choice6.D(d)   => checked_ecast(Choice6.D(d))
        case Choice6.E(e)   => checked_ecast(Choice6.E(e))
        case Choice6.F(f)   => checked_ecast(Choice6.F(f))
    }

    pub def mapB(fn: b -> g \ ef, x: Choice6[a, b, c, d, e, f]): Choice6[a, g, c, d, e, f] \ ef = match x {
        case Choice6.A(a)   => checked_ecast(Choice6.A(a))
        case Choice6.B(b)   => Choice6.B(fn(b))
        case Choice6.C(c)   => checked_ecast(Choice6.C(c))
        case Choice6.D(d)   => checked_ecast(Choice6.D(d))
        case Choice6.E(e)   => checked_ecast(Choice6.E(e))
        case Choice6.F(f)   => checked_ecast(Choice6.F(f))
    }

    pub def mapC(fn: c -> g \ ef, x: Choice6[a, b, c, d, e, f]): Choice6[a, b, g, d, e, f] \ ef = match x {
        case Choice6.A(a)   => checked_ecast(Choice6.A(a))
        case Choice6.B(b)   => checked_ecast(Choice6.B(b))
        case Choice6.C(c)   => Choice6.C(fn(c))
        case Choice6.D(d)   => checked_ecast(Choice6.D(d))
        case Choice6.E(e)   => checked_ecast(Choice6.E(e))
        case Choice6.F(f)   => checked_ecast(Choice6.F(f))
    }

    pub def mapD(fn: d -> g \ ef, x: Choice6[a, b, c, d, e, f]): Choice6[a, b, c, g, e, f] \ ef = match x {
        case Choice6.A(a)   => checked_ecast(Choice6.A(a))
        case Choice6.B(b)   => checked_ecast(Choice6.B(b))
        case Choice6.C(c)   => checked_ecast(Choice6.C(c))
        case Choice6.D(d)   => Choice6.D(fn(d))
        case Choice6.E(e)   => checked_ecast(Choice6.E(e))
        case Choice6.F(f)   => checked_ecast(Choice6.F(f))
    }

    pub def mapE(fn: e -> g \ ef, x: Choice6[a, b, c, d, e, f]): Choice6[a, b, c, d, g, f] \ ef = match x {
        case Choice6.A(a)   => checked_ecast(Choice6.A(a))
        case Choice6.B(b)   => checked_ecast(Choice6.B(b))
        case Choice6.C(c)   => checked_ecast(Choice6.C(c))
        case Choice6.D(d)   => checked_ecast(Choice6.D(d))
        case Choice6.E(e)   => Choice6.E(fn(e))
        case Choice6.F(f)   => checked_ecast(Choice6.F(f))
    }

    pub def mapF(fn: f -> g \ ef, x: Choice6[a, b, c, d, e, f]): Choice6[a, b, c, d, e, g] \ ef = match x {
        case Choice6.A(a)   => checked_ecast(Choice6.A(a))
        case Choice6.B(b)   => checked_ecast(Choice6.B(b))
        case Choice6.C(c)   => checked_ecast(Choice6.C(c))
        case Choice6.D(d)   => checked_ecast(Choice6.D(d))
        case Choice6.E(e)   => checked_ecast(Choice6.E(e))
        case Choice6.F(f)   => Choice6.F(fn(f))
    }

    pub def partition(l: List[Choice6[a, b, c, d, e, f]]): (List[a], List[b], List[c], List[d], List[e], List[f]) =
        def loop(xs, ka, kb, kc, kd, ke, kf) = match xs {
            case Nil     => (ka(Nil), kb(Nil), kc(Nil), kd(Nil), ke(Nil), kf(Nil))
            case x :: rs => match x {
                case Choice6.A(a)   => loop(rs, ks -> ka(a :: ks), kb, kc, kd, ke, kf)
                case Choice6.B(b)   => loop(rs, ka, ks -> kb(b :: ks), kc, kd, ke, kf)
                case Choice6.C(c)   => loop(rs, ka, kb, ks -> kc(c :: ks), kd, ke, kf)
                case Choice6.D(d)   => loop(rs, ka, kb, kc, ks -> kd(d :: ks), ke, kf)
                case Choice6.E(e)   => loop(rs, ka, kb, kc, kd, ks -> ke(e :: ks), kf)
                case Choice6.F(f)   => loop(rs, ka, kb, kc, kd, ke, ks -> kf(f :: ks))
            }
        };
        loop(l, identity, identity, identity, identity, identity, identity)


    pub def allA(l: List[Choice6[a, b, c, d, e, f]]): List[a] =
        def loop(xs, ka) = match xs {
            case Nil     => ka(Nil)
            case x :: rs => match x {
                case Choice6.A(a)   => loop(rs, ks -> ka(a :: ks))
                case _              => loop(rs, ka)
            }
        };
        loop(l, identity)

    pub def allB(l: List[Choice6[a, b, c, d, e, f]]): List[b] =
        def loop(xs, kb) = match xs {
            case Nil     => kb(Nil)
            case x :: rs => match x {
                case Choice6.B(b)   => loop(rs, ks -> kb(b :: ks))
                case _              => loop(rs, kb)
            }
        };
        loop(l, identity)

    pub def allC(l: List[Choice6[a, b, c, d, e, f]]): List[c] =
        def loop(xs, kc) = match xs {
            case Nil     => kc(Nil)
            case x :: rs => match x {
                case Choice6.C(c)   => loop(rs, ks -> kc(c :: ks))
                case _              => loop(rs, kc)
            }
        };
        loop(l, identity)

    pub def allD(l: List[Choice6[a, b, c, d, e, f]]): List[d] =
        def loop(xs, kd) = match xs {
            case Nil     => kd(Nil)
            case x :: rs => match x {
                case Choice6.D(d)   => loop(rs, ks -> kd(d :: ks))
                case _              => loop(rs, kd)
            }
        };
        loop(l, identity)

    pub def allE(l: List[Choice6[a, b, c, d, e, f]]): List[e] =
        def loop(xs, ke) = match xs {
            case Nil     => ke(Nil)
            case x :: rs => match x {
                case Choice6.E(e)   => loop(rs, ks -> ke(e :: ks))
                case _              => loop(rs, ke)
            }
        };
        loop(l, identity)

    pub def allF(l: List[Choice6[a, b, c, d, e, f]]): List[f] =
        def loop(xs, kf) = match xs {
            case Nil     => kf(Nil)
            case x :: rs => match x {
                case Choice6.F(f)   => loop(rs, ks -> kf(f :: ks))
                case _              => loop(rs, kf)
            }
        };
        loop(l, identity)
}

