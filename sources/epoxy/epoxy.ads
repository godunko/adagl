--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces;

package epoxy
  with Preelaborate
is

   type GLboolean is new Interfaces.Integer_32;
   type GLuint is new Interfaces.Unsigned_32;

end epoxy;
