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

mod TestEither {

    use Either.{Left, Right}

    /////////////////////////////////////////////////////////////////////////////
    // isOk                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isLeft01(): Bool = Either.isLeft(Left(1)) == true

    @test
    def isLeft02(): Bool = Either.isLeft(Right(1)) == false

    /////////////////////////////////////////////////////////////////////////////
    // isErr                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isRight01(): Bool = Either.isRight(Left(1)) == false

    @test
    def isRight02(): Bool = Either.isRight(Right(1)) == true

}
