--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body OpenGL.Shaders.Internals is

   -------------------
   -- Get_Shader_Id --
   -------------------

   function Get_Shader_Id
    (Self : not null access OpenGL_Shader'Class) return epoxy.GLuint is
   begin
      return Self.Shader;
   end Get_Shader_Id;

end OpenGL.Shaders.Internals;
