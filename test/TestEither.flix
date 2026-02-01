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

    @Test
    def isLeft01(): Unit \ Assert = 
        Assert.assertEq(expected = true, Either.isLeft(Left(1)))

    @Test
    def isLeft02(): Unit \ Assert = 
        Assert.assertEq(expected = false, Either.isLeft(Right(1)))

    /////////////////////////////////////////////////////////////////////////////
    // isErr                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isRight01(): Unit \ Assert = 
        Assert.assertEq(expected = false, Either.isRight(Left(1)))

    @Test
    def isRight02(): Unit \ Assert = 
        Assert.assertEq(expected = true, Either.isRight(Right(1)))

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefaultLeft                                                      //
    /////////////////////////////////////////////////////////////////////////////
   
    @Test
    def getWithDefaultLeft01(): Unit \ Assert = 
        Assert.assertEq(expected = 1, Either.getWithDefaultLeft(2, Left(1)))

    @Test
    def getWithDefaultLeft02(): Unit \ Assert = 
        Assert.assertEq(expected = 2, Either.getWithDefaultLeft(2, Right(false)))

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefaultRight                                                     //
    /////////////////////////////////////////////////////////////////////////////
   
    @Test
    def getWithDefaultRight01(): Unit \ Assert = 
        Assert.assertEq(expected = 2, Either.getWithDefaultRight(2, Left(1)))

    @Test
    def getWithDefaultRight02(): Unit \ Assert = 
        Assert.assertEq(expected = false, Either.getWithDefaultRight(true, Right(false)))

}
