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


mod Choice2 {

    ///
    /// The Choice2 type.
    ///
    pub enum Choice2[a, b] with Eq, Order, ToString {
        case A(a)
        case B(b)
    }


    ///
    /// Returns `true` iff `x` is `Left(_)`.
    ///
    pub def isA(e: Choice2[a, b]): Bool = match e {
        case Choice2.A(_)   => true
        case _              => false
    }

    ///
    /// Returns `true` iff `e` is `Right(_)`.
    ///
    pub def isB(e: Choice2[a, b]): Bool = match e {
        case Choice2.B(_)   => true
        case _              => false
    }

    pub def getWithDefaultA(d: a, x: Choice2[a, b]): a = match x {
        case Choice2.A(x1)  => x1
        case _              => d
    }

    pub def getWithDefaultB(d: b, x: Choice2[a, b]): b = match x {
        case Choice2.B(x1)  => x1
        case _              => d
    }

    // Wrong name?
    pub def map2(f: a -> c \ ef1, 
                    g: b -> c \ ef2, 
                    x: Choice2[a, b]): c \ {ef1, ef2} = match x {
        case Choice2.A(a)   => checked_ecast(f(a))
        case Choice2.B(b)   => checked_ecast(g(b))
    }

    pub def mapA(f: a -> c \ ef, x: Choice2[a, b]): Choice2[c, b] \ ef = match x {
        case Choice2.A(a)   => Choice2.A(f(a))
        case Choice2.B(b)   => checked_ecast(Choice2.B(b))
    }

    pub def mapB(f: b -> c \ ef, x: Choice2[a, b]): Choice2[a, c] \ ef = match x {
        case Choice2.A(a)   => checked_ecast(Choice2.A(a))
        case Choice2.B(b)   => Choice2.B(f(b))
    }



    pub def partition(l: List[Choice2[a, b]]): (List[a], List[b]) =
        def loop(xs, ka, kb) = match xs {
            case Nil     => (ka(Nil), kb(Nil))
            case x :: rs => match x {
                case Choice2.A(a)   => loop(rs, ks -> ka(a :: ks), kb)
                case Choice2.B(b)   => loop(rs, ka, ks -> kb(b :: ks))
            }
        };
        loop(l, identity, identity)


    pub def allA(l: List[Choice2[a, b]]): List[a] =
        def loop(xs, ka) = match xs {
            case Nil     => ka(Nil)
            case x :: rs => match x {
                case Choice2.A(a)   => loop(rs, ks -> ka(a :: ks))
                case _              => loop(rs, ka)
            }
        };
        loop(l, identity)

    pub def allB(l: List[Choice2[a, b]]): List[b] =
        def loop(xs, kb) = match xs {
            case Nil     => kb(Nil)
            case x :: rs => match x {
                case Choice2.B(b)   => loop(rs, ks -> kb(b :: ks))
                case _              => loop(rs, kb)
            }
        };
        loop(l, identity)

}

