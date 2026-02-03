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


mod Choice7 {

    ///
    /// The Choice7 type.
    ///
    pub enum Choice7[a, b, c, d, e, f, g] with Eq, Order, ToString {
        case A(a)
        case B(b)
        case C(c)
        case D(d)
        case E(e)
        case F(f)
        case G(g)
    }

        ///
    /// Returns `true` iff `x` is `A(_)`.
    ///
    pub def isA(x: Choice7[a, b, c, d, e, f, g]): Bool = match x {
        case Choice7.A(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `B(_)`.
    ///
    pub def isB(x: Choice7[a, b, c, d, e, f, g]): Bool = match x {
        case Choice7.B(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `C(_)`.
    ///
    pub def isC(x: Choice7[a, b, c, d, e, f, g]): Bool = match x {
        case Choice7.C(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `D(_)`.
    ///
    pub def isD(x: Choice7[a, b, c, d, e, f, g]): Bool = match x {
        case Choice7.D(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `E(_)`.
    ///
    pub def isE(x: Choice7[a, b, c, d, e, f, g]): Bool = match x {
        case Choice7.E(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `F(_)`.
    ///
    pub def isF(x: Choice7[a, b, c, d, e, f, g]): Bool = match x {
        case Choice7.F(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `x` is `G(_)`.
    ///
    pub def isG(x: Choice7[a, b, c, d, e, f, g]): Bool = match x {
        case Choice7.G(_)   => true
        case _              => false
    }



    pub def getWithDefaultA(d: a, x: Choice7[a, b, c, d, e, f, g]): a = match x {
        case Choice7.A(x1)  => x1
        case _              => d
    }



    pub def getWithDefaultB(d: b, x: Choice7[a, b, c, d, e, f, g]): b = match x {
        case Choice7.B(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultC(d: c, x: Choice7[a, b, c, d, e, f, g]): c = match x {
        case Choice7.C(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultD(d: d, x: Choice7[a, b, c, d, e, f, g]): d = match x {
        case Choice7.D(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultE(d: e, x: Choice7[a, b, c, d, e, f, g]): e = match x {
        case Choice7.E(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultF(d: f, x: Choice7[a, b, c, d, e, f, g]): f = match x {
        case Choice7.F(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultG(d: g, x: Choice7[a, b, c, d, e, f, g]): g = match x {
        case Choice7.G(x1)  => x1
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
    //                 x: Choice7[a, b, c, d, e, f, g]): h \ {ef1, ef2, ef3, ef4, ef5, ef6, ef7} = match x {
    //     case Choice7.A(a)   => checked_ecast(f1(a))
    //     case Choice7.B(b)   => checked_ecast(f2(b))
    //     case Choice7.C(c)   => checked_ecast(f3(c))
    //     case Choice7.D(d)   => checked_ecast(f4(d))
    //     case Choice7.E(e)   => checked_ecast(f5(e))
    //     case Choice7.F(f)   => checked_ecast(f6(f))
    //     case Choice7.G(g)   => checked_ecast(f7(g))
    // }


    pub def mapA(fn: a -> h \ ef, x: Choice7[a, b, c, d, e, f, g]): Choice7[h, b, c, d, e, f, g] \ ef = match x {
        case Choice7.A(a)   => Choice7.A(fn(a))
        case Choice7.B(b)   => checked_ecast(Choice7.B(b))
        case Choice7.C(c)   => checked_ecast(Choice7.C(c))
        case Choice7.D(d)   => checked_ecast(Choice7.D(d))
        case Choice7.E(e)   => checked_ecast(Choice7.E(e))
        case Choice7.F(f)   => checked_ecast(Choice7.F(f))
        case Choice7.G(g)   => checked_ecast(Choice7.G(g))
    }


    pub def mapB(fn: b -> h \ ef, x: Choice7[a, b, c, d, e, f, g]): Choice7[a, h, c, d, e, f, g] \ ef = match x {
        case Choice7.A(a)   => checked_ecast(Choice7.A(a))
        case Choice7.B(b)   => Choice7.B(fn(b))
        case Choice7.C(c)   => checked_ecast(Choice7.C(c))
        case Choice7.D(d)   => checked_ecast(Choice7.D(d))
        case Choice7.E(e)   => checked_ecast(Choice7.E(e))
        case Choice7.F(f)   => checked_ecast(Choice7.F(f))
        case Choice7.G(g)   => checked_ecast(Choice7.G(g))
    }

    pub def mapC(fn: c -> h \ ef, x: Choice7[a, b, c, d, e, f, g]): Choice7[a, b, h, d, e, f, g] \ ef = match x {
        case Choice7.A(a)   => checked_ecast(Choice7.A(a))
        case Choice7.B(b)   => checked_ecast(Choice7.B(b))
        case Choice7.C(c)   => Choice7.C(fn(c))
        case Choice7.D(d)   => checked_ecast(Choice7.D(d))
        case Choice7.E(e)   => checked_ecast(Choice7.E(e))
        case Choice7.F(f)   => checked_ecast(Choice7.F(f))
        case Choice7.G(g)   => checked_ecast(Choice7.G(g))
    }

    pub def mapD(fn: d -> h \ ef, x: Choice7[a, b, c, d, e, f, g]): Choice7[a, b, c, h, e, f, g] \ ef = match x {
        case Choice7.A(a)   => checked_ecast(Choice7.A(a))
        case Choice7.B(b)   => checked_ecast(Choice7.B(b))
        case Choice7.C(c)   => checked_ecast(Choice7.C(c))
        case Choice7.D(d)   => Choice7.D(fn(d))
        case Choice7.E(e)   => checked_ecast(Choice7.E(e))
        case Choice7.F(f)   => checked_ecast(Choice7.F(f))
        case Choice7.G(g)   => checked_ecast(Choice7.G(g))
    }

    pub def mapE(fn: e -> h \ ef, x: Choice7[a, b, c, d, e, f, g]): Choice7[a, b, c, d, h, f, g] \ ef = match x {
        case Choice7.A(a)   => checked_ecast(Choice7.A(a))
        case Choice7.B(b)   => checked_ecast(Choice7.B(b))
        case Choice7.C(c)   => checked_ecast(Choice7.C(c))
        case Choice7.D(d)   => checked_ecast(Choice7.D(d))
        case Choice7.E(e)   => Choice7.E(fn(e))
        case Choice7.F(f)   => checked_ecast(Choice7.F(f))
        case Choice7.G(g)   => checked_ecast(Choice7.G(g))
    }

    pub def mapF(fn: f -> h \ ef, x: Choice7[a, b, c, d, e, f, g]): Choice7[a, b, c, d, e, h, g] \ ef = match x {
        case Choice7.A(a)   => checked_ecast(Choice7.A(a))
        case Choice7.B(b)   => checked_ecast(Choice7.B(b))
        case Choice7.C(c)   => checked_ecast(Choice7.C(c))
        case Choice7.D(d)   => checked_ecast(Choice7.D(d))
        case Choice7.E(e)   => checked_ecast(Choice7.E(e))
        case Choice7.F(f)   => Choice7.F(fn(f))
        case Choice7.G(g)   => checked_ecast(Choice7.G(g))
    }

    pub def mapG(fn: g -> h \ ef, x: Choice7[a, b, c, d, e, f, g]): Choice7[a, b, c, d, e, f, h] \ ef = match x {
        case Choice7.A(a)   => checked_ecast(Choice7.A(a))
        case Choice7.B(b)   => checked_ecast(Choice7.B(b))
        case Choice7.C(c)   => checked_ecast(Choice7.C(c))
        case Choice7.D(d)   => checked_ecast(Choice7.D(d))
        case Choice7.E(e)   => checked_ecast(Choice7.E(e))
        case Choice7.F(f)   => checked_ecast(Choice7.F(f))
        case Choice7.G(g)   => Choice7.G(fn(g))
    }


    pub def partition(l: List[Choice7[a, b, c, d, e, f, g]]): (List[a], List[b], List[c], List[d], List[e], List[f], List[g]) =
        def loop(xs, ka, kb, kc, kd, ke, kf, kg) = match xs {
            case Nil     => (ka(Nil), kb(Nil), kc(Nil), kd(Nil), ke(Nil), kf(Nil), kg(Nil))
            case x :: rs => match x {
                case Choice7.A(a)   => loop(rs, ks -> ka(a :: ks), kb, kc, kd, ke, kf, kg)
                case Choice7.B(b)   => loop(rs, ka, ks -> kb(b :: ks), kc, kd, ke, kf, kg)
                case Choice7.C(c)   => loop(rs, ka, kb, ks -> kc(c :: ks), kd, ke, kf, kg)
                case Choice7.D(d)   => loop(rs, ka, kb, kc, ks -> kd(d :: ks), ke, kf, kg)
                case Choice7.E(e)   => loop(rs, ka, kb, kc, kd, ks -> ke(e :: ks), kf, kg)
                case Choice7.F(f)   => loop(rs, ka, kb, kc, kd, ke, ks -> kf(f :: ks), kg)
                case Choice7.G(g)   => loop(rs, ka, kb, kc, kd, ke, kf, ks -> kg(g :: ks))
            }
        };
        loop(l, identity, identity, identity, identity, identity, identity, identity)




    pub def allA(l: List[Choice7[a, b, c, d, e, f, g]]): List[a] =
        def loop(xs, ka) = match xs {
            case Nil     => ka(Nil)
            case x :: rs => match x {
                case Choice7.A(a)   => loop(rs, ks -> ka(a :: ks))
                case _              => loop(rs, ka)
            }
        };
        loop(l, identity)

    pub def allB(l: List[Choice7[a, b, c, d, e, f, g]]): List[b] =
        def loop(xs, kb) = match xs {
            case Nil     => kb(Nil)
            case x :: rs => match x {
                case Choice7.B(b)   => loop(rs, ks -> kb(b :: ks))
                case _              => loop(rs, kb)
            }
        };
        loop(l, identity)

    pub def allC(l: List[Choice7[a, b, c, d, e, f, g]]): List[c] =
        def loop(xs, kc) = match xs {
            case Nil     => kc(Nil)
            case x :: rs => match x {
                case Choice7.C(c)   => loop(rs, ks -> kc(c :: ks))
                case _              => loop(rs, kc)
            }
        };
        loop(l, identity)

    pub def allD(l: List[Choice7[a, b, c, d, e, f, g]]): List[d] =
        def loop(xs, kd) = match xs {
            case Nil     => kd(Nil)
            case x :: rs => match x {
                case Choice7.D(d)   => loop(rs, ks -> kd(d :: ks))
                case _              => loop(rs, kd)
            }
        };
        loop(l, identity)

    pub def allE(l: List[Choice7[a, b, c, d, e, f, g]]): List[e] =
        def loop(xs, ke) = match xs {
            case Nil     => ke(Nil)
            case x :: rs => match x {
                case Choice7.E(e)   => loop(rs, ks -> ke(e :: ks))
                case _              => loop(rs, ke)
            }
        };
        loop(l, identity)

    pub def allF(l: List[Choice7[a, b, c, d, e, f, g]]): List[f] =
        def loop(xs, kf) = match xs {
            case Nil     => kf(Nil)
            case x :: rs => match x {
                case Choice7.F(f)   => loop(rs, ks -> kf(f :: ks))
                case _              => loop(rs, kf)
            }
        };
        loop(l, identity)

    pub def allG(l: List[Choice7[a, b, c, d, e, f, g]]): List[g] =
        def loop(xs, kg) = match xs {
            case Nil     => kg(Nil)
            case x :: rs => match x {
                case Choice7.G(g)   => loop(rs, ks -> kg(g :: ks))
                case _              => loop(rs, kg)
            }
        };
        loop(l, identity)


}

