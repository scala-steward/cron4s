/*
 * Copyright 2017 Antonio Alonso Dominguez
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cron4s

import cron4s.parser.CronExpr

package object parsing {

  @deprecated(
    message = "Parser-Combinator parser in deprecated in favor of atto parser",
    since = "0.8.0"
  )
  object Parser extends cron4s.parser.Parser {

    override def parse(input: String): Either[parser.Error, CronExpr] =
      for {
        tokens <- CronLexer.tokenize(input)
        expr   <- CronParser.read(tokens)
      } yield expr

  }

}
