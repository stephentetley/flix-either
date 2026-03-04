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


pub mod Choice8 {

    ///
    /// The Choice8 type.
    ///
    pub enum Choice8[a, b, c, d, e, f, g, h] with Eq, Order, ToString {
        case A(a)
        case B(b)
        case C(c)
        case D(d)
        case E(e)
        case F(f)
        case G(g)
        case H(h)
    }

        ///
    /// Returns `true` iff `x` is `A(_)`.
    ///
    pub def isA(x: Choice8[a, b, c, d, e, f, g, h]): Bool = match x {
        case Choice8.A(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `B(_)`.
    ///
    pub def isB(x: Choice8[a, b, c, d, e, f, g, h]): Bool = match x {
        case Choice8.B(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `C(_)`.
    ///
    pub def isC(x: Choice8[a, b, c, d, e, f, g, h]): Bool = match x {
        case Choice8.C(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `D(_)`.
    ///
    pub def isD(x: Choice8[a, b, c, d, e, f, g, h]): Bool = match x {
        case Choice8.D(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `E(_)`.
    ///
    pub def isE(x: Choice8[a, b, c, d, e, f, g, h]): Bool = match x {
        case Choice8.E(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `F(_)`.
    ///
    pub def isF(x: Choice8[a, b, c, d, e, f, g, h]): Bool = match x {
        case Choice8.F(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `G(_)`.
    ///
    pub def isG(x: Choice8[a, b, c, d, e, f, g, h]): Bool = match x {
        case Choice8.G(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `G(_)`.
    ///
    pub def isH(x: Choice8[a, b, c, d, e, f, g, h]): Bool = match x {
        case Choice8.H(_)   => true
        case _              => false
    }


    pub def getWithDefaultA(d: a, x: Choice8[a, b, c, d, e, f, g, h]): a = match x {
        case Choice8.A(x1)  => x1
        case _              => d
    }



    pub def getWithDefaultB(d: b, x: Choice8[a, b, c, d, e, f, g, h]): b = match x {
        case Choice8.B(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultC(d: c, x: Choice8[a, b, c, d, e, f, g, h]): c = match x {
        case Choice8.C(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultD(d: d, x: Choice8[a, b, c, d, e, f, g, h]): d = match x {
        case Choice8.D(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultE(d: e, x: Choice8[a, b, c, d, e, f, g, h]): e = match x {
        case Choice8.E(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultF(d: f, x: Choice8[a, b, c, d, e, f, g, h]): f = match x {
        case Choice8.F(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultG(d: g, x: Choice8[a, b, c, d, e, f, g, h]): g = match x {
        case Choice8.G(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultH(d: h, x: Choice8[a, b, c, d, e, f, g, h]): h = match x {
        case Choice8.H(x1)  => x1
        case _              => d
    }

    // // Wrong name?
    // pub def map7(f1: a -> h \ ef1,
    //                 f2: b -> h \ ef2,
    //                 f3: c -> h \ ef3,
    //                 f4: d -> h \ ef4,
    //                 f5: e -> h \ ef5,
    //                 f6: f -> h \ ef6,
    //                 f7: g -> h \ ef7,
    //                 x: Choice8[a, b, c, d, e, f, g, h]): h \ {ef1, ef2, ef3, ef4, ef5, ef6, ef7} = match x {
    //     case Choice8.A(a)   => checked_ecast(f1(a))
    //     case Choice8.B(b)   => checked_ecast(f2(b))
    //     case Choice8.C(c)   => checked_ecast(f3(c))
    //     case Choice8.D(d)   => checked_ecast(f4(d))
    //     case Choice8.E(e)   => checked_ecast(f5(e))
    //     case Choice8.F(f)   => checked_ecast(f6(f))
    //     case Choice8.G(g)   => checked_ecast(f7(g))
    // }


    pub def mapA(fn: a -> i \ ef, x: Choice8[a, b, c, d, e, f, g, h]): Choice8[i, b, c, d, e, f, g, h] \ ef = match x {
        case Choice8.A(a)   => Choice8.A(fn(a))
        case Choice8.B(b)   => checked_ecast(Choice8.B(b))
        case Choice8.C(c)   => checked_ecast(Choice8.C(c))
        case Choice8.D(d)   => checked_ecast(Choice8.D(d))
        case Choice8.E(e)   => checked_ecast(Choice8.E(e))
        case Choice8.F(f)   => checked_ecast(Choice8.F(f))
        case Choice8.G(g)   => checked_ecast(Choice8.G(g))
        case Choice8.H(h)   => checked_ecast(Choice8.H(h))
    }


    pub def mapB(fn: b -> i \ ef, x: Choice8[a, b, c, d, e, f, g, h]): Choice8[a, i, c, d, e, f, g, h] \ ef = match x {
        case Choice8.A(a)   => checked_ecast(Choice8.A(a))
        case Choice8.B(b)   => Choice8.B(fn(b))
        case Choice8.C(c)   => checked_ecast(Choice8.C(c))
        case Choice8.D(d)   => checked_ecast(Choice8.D(d))
        case Choice8.E(e)   => checked_ecast(Choice8.E(e))
        case Choice8.F(f)   => checked_ecast(Choice8.F(f))
        case Choice8.G(g)   => checked_ecast(Choice8.G(g))
        case Choice8.H(h)   => checked_ecast(Choice8.H(h))
    }

    pub def mapC(fn: c -> i \ ef, x: Choice8[a, b, c, d, e, f, g, h]): Choice8[a, b, i, d, e, f, g, h] \ ef = match x {
        case Choice8.A(a)   => checked_ecast(Choice8.A(a))
        case Choice8.B(b)   => checked_ecast(Choice8.B(b))
        case Choice8.C(c)   => Choice8.C(fn(c))
        case Choice8.D(d)   => checked_ecast(Choice8.D(d))
        case Choice8.E(e)   => checked_ecast(Choice8.E(e))
        case Choice8.F(f)   => checked_ecast(Choice8.F(f))
        case Choice8.G(g)   => checked_ecast(Choice8.G(g))
        case Choice8.H(h)   => checked_ecast(Choice8.H(h))
    }

    pub def mapD(fn: d -> i \ ef, x: Choice8[a, b, c, d, e, f, g, h]): Choice8[a, b, c, i, e, f, g, h] \ ef = match x {
        case Choice8.A(a)   => checked_ecast(Choice8.A(a))
        case Choice8.B(b)   => checked_ecast(Choice8.B(b))
        case Choice8.C(c)   => checked_ecast(Choice8.C(c))
        case Choice8.D(d)   => Choice8.D(fn(d))
        case Choice8.E(e)   => checked_ecast(Choice8.E(e))
        case Choice8.F(f)   => checked_ecast(Choice8.F(f))
        case Choice8.G(g)   => checked_ecast(Choice8.G(g))
        case Choice8.H(h)   => checked_ecast(Choice8.H(h))
    }

    pub def mapE(fn: e -> i \ ef, x: Choice8[a, b, c, d, e, f, g, h]): Choice8[a, b, c, d, i, f, g, h] \ ef = match x {
        case Choice8.A(a)   => checked_ecast(Choice8.A(a))
        case Choice8.B(b)   => checked_ecast(Choice8.B(b))
        case Choice8.C(c)   => checked_ecast(Choice8.C(c))
        case Choice8.D(d)   => checked_ecast(Choice8.D(d))
        case Choice8.E(e)   => Choice8.E(fn(e))
        case Choice8.F(f)   => checked_ecast(Choice8.F(f))
        case Choice8.G(g)   => checked_ecast(Choice8.G(g))
        case Choice8.H(h)   => checked_ecast(Choice8.H(h))
    }

    pub def mapF(fn: f -> i \ ef, x: Choice8[a, b, c, d, e, f, g, h]): Choice8[a, b, c, d, e, i, g, h] \ ef = match x {
        case Choice8.A(a)   => checked_ecast(Choice8.A(a))
        case Choice8.B(b)   => checked_ecast(Choice8.B(b))
        case Choice8.C(c)   => checked_ecast(Choice8.C(c))
        case Choice8.D(d)   => checked_ecast(Choice8.D(d))
        case Choice8.E(e)   => checked_ecast(Choice8.E(e))
        case Choice8.F(f)   => Choice8.F(fn(f))
        case Choice8.G(g)   => checked_ecast(Choice8.G(g))
        case Choice8.H(h)   => checked_ecast(Choice8.H(h))
    }

    pub def mapG(fn: g -> i \ ef, x: Choice8[a, b, c, d, e, f, g, h]): Choice8[a, b, c, d, e, f, i, h] \ ef = match x {
        case Choice8.A(a)   => checked_ecast(Choice8.A(a))
        case Choice8.B(b)   => checked_ecast(Choice8.B(b))
        case Choice8.C(c)   => checked_ecast(Choice8.C(c))
        case Choice8.D(d)   => checked_ecast(Choice8.D(d))
        case Choice8.E(e)   => checked_ecast(Choice8.E(e))
        case Choice8.F(f)   => checked_ecast(Choice8.F(f))
        case Choice8.G(g)   => Choice8.G(fn(g))
        case Choice8.H(h)   => checked_ecast(Choice8.H(h))
    }

    pub def mapH(fn: h -> i \ ef, x: Choice8[a, b, c, d, e, f, g, h]): Choice8[a, b, c, d, e, f, g, i] \ ef = match x {
        case Choice8.A(a)   => checked_ecast(Choice8.A(a))
        case Choice8.B(b)   => checked_ecast(Choice8.B(b))
        case Choice8.C(c)   => checked_ecast(Choice8.C(c))
        case Choice8.D(d)   => checked_ecast(Choice8.D(d))
        case Choice8.E(e)   => checked_ecast(Choice8.E(e))
        case Choice8.F(f)   => checked_ecast(Choice8.F(f))
        case Choice8.G(g)   => checked_ecast(Choice8.G(g))
        case Choice8.H(h)   => Choice8.H(fn(h))
    }

    pub def partition(l: List[Choice8[a, b, c, d, e, f, g, h]]): (List[a], List[b], List[c], List[d], List[e], List[f], List[g], List[h]) =
        def loop(xs, ka, kb, kc, kd, ke, kf, kg, kh) = match xs {
            case Nil     => (ka(Nil), kb(Nil), kc(Nil), kd(Nil), ke(Nil), kf(Nil), kg(Nil), kh(Nil))
            case x :: rs => match x {
                case Choice8.A(a)   => loop(rs, ks -> ka(a :: ks), kb, kc, kd, ke, kf, kg, kh)
                case Choice8.B(b)   => loop(rs, ka, ks -> kb(b :: ks), kc, kd, ke, kf, kg, kh)
                case Choice8.C(c)   => loop(rs, ka, kb, ks -> kc(c :: ks), kd, ke, kf, kg, kh)
                case Choice8.D(d)   => loop(rs, ka, kb, kc, ks -> kd(d :: ks), ke, kf, kg, kh)
                case Choice8.E(e)   => loop(rs, ka, kb, kc, kd, ks -> ke(e :: ks), kf, kg, kh)
                case Choice8.F(f)   => loop(rs, ka, kb, kc, kd, ke, ks -> kf(f :: ks), kg, kh)
                case Choice8.G(g)   => loop(rs, ka, kb, kc, kd, ke, kf, ks -> kg(g :: ks), kh)
                case Choice8.H(h)   => loop(rs, ka, kb, kc, kd, ke, kf, kg, ks -> kh(h :: ks))
            }
        };
        loop(l, identity, identity, identity, identity, identity, identity, identity, identity)




    pub def allA(l: List[Choice8[a, b, c, d, e, f, g, h]]): List[a] =
        def loop(xs, ka) = match xs {
            case Nil     => ka(Nil)
            case x :: rs => match x {
                case Choice8.A(a)   => loop(rs, ks -> ka(a :: ks))
                case _              => loop(rs, ka)
            }
        };
        loop(l, identity)

    pub def allB(l: List[Choice8[a, b, c, d, e, f, g, h]]): List[b] =
        def loop(xs, kb) = match xs {
            case Nil     => kb(Nil)
            case x :: rs => match x {
                case Choice8.B(b)   => loop(rs, ks -> kb(b :: ks))
                case _              => loop(rs, kb)
            }
        };
        loop(l, identity)

    pub def allC(l: List[Choice8[a, b, c, d, e, f, g, h]]): List[c] =
        def loop(xs, kc) = match xs {
            case Nil     => kc(Nil)
            case x :: rs => match x {
                case Choice8.C(c)   => loop(rs, ks -> kc(c :: ks))
                case _              => loop(rs, kc)
            }
        };
        loop(l, identity)

    pub def allD(l: List[Choice8[a, b, c, d, e, f, g, h]]): List[d] =
        def loop(xs, kd) = match xs {
            case Nil     => kd(Nil)
            case x :: rs => match x {
                case Choice8.D(d)   => loop(rs, ks -> kd(d :: ks))
                case _              => loop(rs, kd)
            }
        };
        loop(l, identity)

    pub def allE(l: List[Choice8[a, b, c, d, e, f, g, h]]): List[e] =
        def loop(xs, ke) = match xs {
            case Nil     => ke(Nil)
            case x :: rs => match x {
                case Choice8.E(e)   => loop(rs, ks -> ke(e :: ks))
                case _              => loop(rs, ke)
            }
        };
        loop(l, identity)

    pub def allF(l: List[Choice8[a, b, c, d, e, f, g, h]]): List[f] =
        def loop(xs, kf) = match xs {
            case Nil     => kf(Nil)
            case x :: rs => match x {
                case Choice8.F(f)   => loop(rs, ks -> kf(f :: ks))
                case _              => loop(rs, kf)
            }
        };
        loop(l, identity)

    pub def allG(l: List[Choice8[a, b, c, d, e, f, g, h]]): List[g] =
        def loop(xs, kg) = match xs {
            case Nil     => kg(Nil)
            case x :: rs => match x {
                case Choice8.G(g)   => loop(rs, ks -> kg(g :: ks))
                case _              => loop(rs, kg)
            }
        };
        loop(l, identity)

    pub def allH(l: List[Choice8[a, b, c, d, e, f, g, h]]): List[h] =
        def loop(xs, kh) = match xs {
            case Nil     => kh(Nil)
            case x :: rs => match x {
                case Choice8.H(h)   => loop(rs, ks -> kh(h:: ks))
                case _              => loop(rs, kh)
            }
        };
        loop(l, identity)


}

