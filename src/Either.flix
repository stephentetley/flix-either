/*
 * Copyright 2023 Stephen Tetley
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

///
/// The Either type.
///
pub enum Either[a, b] with Eq, Order, ToString, Sendable {
    case Left(a)
    case Right(b)
}

instance Hash[Either[a, b]] with Hash[a], Hash[b] {
    pub def hash(e: Either[a, b]): Int32 = match e {
        case Either.Left(a)  => 5381 + 113 * Hash.hash(a)
        case Either.Right(b) => 5351 + 97 * Hash.hash(b)
    }
}


instance Functor[Either[e]] {
    pub def map(f: b -> c \ ef, x: Either[a, b]): Either[a, c] \ ef = Either.mapRight(f, x)
}

// instance Applicative[Either[e]] {
//     pub def point(x: a): Either[e, a] = Right(x)

//     pub def ap(mf: Either[e, a -> b & ef], mx: Either[e, a]): Either[e, b] & ef = match mf {
//         case Right(f) => match mx {
//             case Right(x) => Right(f(x))
//             case Left(a1) => Left(a1)
//         }
//         case Left(a) => Left(a)
//     }
// }

// instance Monad[Either[e]] {
//     pub def flatMap(k: a -> Either[e, b] & ef, ma: Either[e, a]): Either[e, b] & ef = match ma {
//         case Right(a) => k(a)
//         case Left(e) => Left(e) as & ef
//     }
// }


// instance Foldable[Either[e]] {
//     pub def foldLeft(f: (b, a) -> b & ef, s: b, t: Either[e, a]): b & ef = 
//         match t {
//             case Left(_) => s
//             case Right(a) => f(s, a)
//         }

//     pub def foldRight(f: (a, b) -> b & ef, s: b, t: Either[e, a]): b & ef = 
//         match t {
//             case Left(_) => s
//             case Right(a) => f(a, s)
//         }

//     pub def foldRightWithCont(f: (a, Unit -> b & ef) -> b & ef, s: b, t: Either[e, a]): b & ef = 
//         match t {
//             case Left(_) => s
//             case Right(a) => f(a, _ -> s as & ef)
//         }

// }

// instance Traversable[Either[e]] {
//     pub def traverse(f: a -> m[b] & ef, t: Either[e, a]): m[Either[e, b]] & ef with Applicative[m] =
//         use Applicative.{point};
//         match t {
//             case Left(e) => point(Left(e))
//             case Right(a) => Functor.map(Right, f(a))
//         }

//     pub override def sequence(t: Either[e, m[a]]): m[Either[e, a]] with Applicative[m] =
//         use Applicative.{point};
//         match t {
//             case Left(e) => point(Left(e))
//             case Right(ma) => Functor.map(Right, ma)
//         }
// }




mod Either {

    use Either
    use Either.{Left, Right}

    ///
    /// Returns `true` iff `x` is `Left(_)`.
    ///
    pub def isLeft(e: Either[a, b]): Bool = match e {
        case Left(_)  => true
        case Right(_) => false
    }

    ///
    /// Returns `true` iff `e` is `Right(_)`.
    ///
    pub def isRight(e: Either[a, b]): Bool = match e {
        case Left(_)  => false
        case Right(_) => true
    }


    pub def either(f: a -> c \ ef1, g: b -> c \ ef2, x: Either[a, b]): c \ {ef1, ef2} = match x {
        case Left(a)  => checked_ecast(f(a))
        case Right(b) => checked_ecast(g(b))
    }

    pub def mapLeft(f: a -> c \ ef, x: Either[a, b]): Either[c, b] \ ef = match x {
        case Left(a)  => Left(f(a))
        case Right(b) => checked_ecast(Right(b))
    }

    pub def mapRight(g: b -> c \ ef, x: Either[a, b]): Either[a, c] \ ef = match x {
        case Left(a)  => checked_ecast(Left(a))
        case Right(b) => Right(g(b))
    }

    pub def fromLeft(d: a, e: Either[a, b]): a = match e {
        case Left(a) => a
        case Right(_) => d
    }

    pub def fromRight(d: b, e: Either[a ,b]): b = match e {
        case Left(_) => d
        case Right(b) => b
    }

    pub def partition(l: List[Either[a, b]]): (List[a], List[b]) =
        def loop(xs, k1, k2) = match xs {
            case Nil     => (k1(Nil), k2(Nil))
            case x :: rs => match x {
                case Left(a)  => loop(rs, ys -> k1(a :: ys), k2)
                case Right(b) => loop(rs, k1, zs -> k2(b :: zs))
            }
        };
        loop(l, identity, identity)


    pub def lefts(l: List[Either[a, b]]): List[a] =
        def loop(xs, k) = match xs {
            case Nil     => k(Nil)
            case x :: rs => match x {
                case Left(a)  => loop(rs, zs -> k(a :: zs))
                case Right(_) => loop(rs, k)
            }
        };
        loop(l, identity)

    pub def rights(l: List[Either[a,b]]): List[b] =
        def loop(xs, k) = match xs {
            case Nil     => k(Nil)
            case x :: rs => match x {
                case Left(_)  => loop(rs, k)
                case Right(b) => loop(rs, zs -> k(b :: zs))
            }
        };
        loop(l, identity)


}
