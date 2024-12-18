--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with epoxy;

package OpenGL.Shaders.Internals is

   function Get_Shader_Id
     (Self : not null access OpenGL_Shader'Class) return epoxy.GLuint;

end OpenGL.Shaders.Internals;
