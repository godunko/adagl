------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2018-2020, Vadim Godunko <vgodunko@gmail.com>                --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Vadim Godunko, IE nor the names of its        --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------

with OpenGL;
with Interfaces.C.Strings;
with System;

package GLEW is

   pragma Preelaborate;

   type GLuint is new Interfaces.Unsigned_32;
   type GLuint_Array is array (Positive range <>) of GLuint
     with Convention => C;
   type GLint_Array is array (Positive range <>) of OpenGL.GLint
     with Convention => C;

   type glewGenBuffers is access procedure
     (n       : OpenGL.GLsizei;
      buffers : access GLuint) with Convention => C;

   glGenBuffers : glewGenBuffers
     with Import, Convention => C, External_Name => "__glewGenBuffers";

   type glewBufferData is access procedure
     (target : OpenGL.GLenum;
      size   : Interfaces.C.ptrdiff_t;
      data   : System.Address;
      buffer : GLuint) with Convention => C;

   glBufferData : glewBufferData
     with Import, Convention => C, External_Name => "__glewBufferData";

   type glewBindBuffer is access procedure
     (target : OpenGL.GLenum;
      buffer : GLuint) with Convention => C;

   glBindBuffer : glewBindBuffer
     with Import, Convention => C, External_Name => "__glewBindBuffer";

   type glewCreateShader is access function
     (shaderType : OpenGL.GLenum) return GLuint with Convention => C;

   glCreateShader : glewCreateShader
     with Import, Convention => C, External_Name => "__glewCreateShader";

   type glewShaderSource is access procedure
     (shader : GLuint;
      count  : OpenGL.GLsizei;
      string : Interfaces.C.Strings.chars_ptr_array;
      length : GLint_Array) with Convention => C;

   glShaderSource : glewShaderSource
     with Import, Convention => C, External_Name => "__glewShaderSource";

   type glewCompileShader is access procedure (shader : GLuint)
     with Convention => C;

   glCompileShader : glewCompileShader
     with Import, Convention => C, External_Name => "__glewCompileShader";

   type glewGetShaderiv is access procedure
     (shader : GLuint;
      pname  : OpenGL.GLenum;
      params : access OpenGL.GLint) with Convention => C;

   glGetShaderiv : glewGetShaderiv
     with Import, Convention => C, External_Name => "__glewGetShaderiv";

   type glewDeleteShader is access procedure (shader : GLuint)
     with Convention => C;

   glDeleteShader : glewDeleteShader
     with Import, Convention => C, External_Name => "__glewDeleteShader";

   type glewGetShaderInfoLog is access procedure
     (shader    : GLuint;
      maxLength : OpenGL.GLsizei;
      length    : access OpenGL.GLsizei;
      infoLog   : out Interfaces.C.char_array)
     with Convention => C;

   glGetShaderInfoLog : glewGetShaderInfoLog
     with Import, Convention => C, External_Name => "__glewGetShaderInfoLog";

   type glewAttachShader is access procedure
     (program   : GLuint;
      shader    : GLuint)
     with Convention => C;

   glAttachShader : glewAttachShader
     with Import, Convention => C, External_Name => "__glewAttachShader";

   type glewGetAttribLocation is access function
     (program : GLuint;
      name    : Interfaces.C.char_array) return OpenGL.GLint
        with Convention => C;

   glGetAttribLocation : glewGetAttribLocation
     with Import, Convention => C, External_Name => "__glewGetAttribLocation";

   type glewUseProgram is access procedure (program   : GLuint)
     with Convention => C;

   glUseProgram : glewUseProgram
     with Import, Convention => C, External_Name => "__glewUseProgram";

   type glewCreateProgram is access function return GLuint
     with Convention => C;

   glCreateProgram : glewCreateProgram
     with Import, Convention => C, External_Name => "__glewCreateProgram";

   type glewDisableVertexAttribArray is access procedure (index : GLuint)
     with Convention => C;

   glDisableVertexAttribArray : glewDisableVertexAttribArray
     with Import, Convention => C,
       External_Name => "__glewDisableVertexAttribArray";

   type glewEnableVertexAttribArray is access procedure (index : GLuint)
     with Convention => C;

   glEnableVertexAttribArray : glewEnableVertexAttribArray
     with Import, Convention => C,
       External_Name => "__glewEnableVertexAttribArray";

   type glewGetProgramiv is access procedure
     (program : GLuint;
      pname   : OpenGL.GLenum;
      params  : access OpenGL.GLint) with Convention => C;

   glGetProgramiv : glewGetProgramiv
     with Import, Convention => C, External_Name => "__glewGetProgramiv";

   type glewLinkProgram is access procedure (index : GLuint)
     with Convention => C;

   glLinkProgram : glewLinkProgram
     with Import, Convention => C, External_Name => "__glewLinkProgram";

   type glewVertexAttribPointer is access procedure
     (index      : GLuint;
      size       : OpenGL.GLint;
      kind       : OpenGL.GLenum;
      normalized : OpenGL.GLint;
      stride     : OpenGL.GLsizei;
      pointer    : System.Address)
     with Convention => C;

   glVertexAttribPointer : glewVertexAttribPointer
     with Import, Convention => C, External_Name => "__glewVertexAttribPointer";

   type glewVertexAttrib1f is access procedure
     (index : GLuint;
      v0    : OpenGL.GLfloat)
     with Convention => C;

   glVertexAttrib1f : glewVertexAttrib1f
     with Import, Convention => C, External_Name => "__glewVertexAttrib1f";

   type glewVertexAttrib2f is access procedure
     (index : GLuint;
      v0    : OpenGL.GLfloat;
      v1    : OpenGL.GLfloat)
     with Convention => C;

   glVertexAttrib2f : glewVertexAttrib2f
     with Import, Convention => C, External_Name => "__glewVertexAttrib2f";

   type glewVertexAttrib3f is access procedure
     (index : GLuint;
      v0    : OpenGL.GLfloat;
      v1    : OpenGL.GLfloat;
      v2    : OpenGL.GLfloat)
     with Convention => C;

   glVertexAttrib3f : glewVertexAttrib3f
     with Import, Convention => C, External_Name => "__glewVertexAttrib3f";

   type glewVertexAttrib4f is access procedure
     (index : GLuint;
      v0    : OpenGL.GLfloat;
      v1    : OpenGL.GLfloat;
      v2    : OpenGL.GLfloat;
      v4    : OpenGL.GLfloat)
     with Convention => C;

   glVertexAttrib4f : glewVertexAttrib4f
     with Import, Convention => C, External_Name => "__glewVertexAttrib4f";

   type glewVertexAttrib2fv is access procedure
     (index : GLuint;
      v     : OpenGL.GLfloat_Matrix_2x2)
     with Convention => C;

   glVertexAttrib2fv : glewVertexAttrib2fv
     with Import, Convention => C, External_Name => "__glewVertexAttrib2fv";

   type glewVertexAttrib3fv is access procedure
     (index : GLuint;
      v     : OpenGL.GLfloat_Matrix_3x3)
     with Convention => C;

   glVertexAttrib3fv : glewVertexAttrib3fv
     with Import, Convention => C, External_Name => "__glewVertexAttrib3fv";

   type glewVertexAttrib4fv is access procedure
     (index : GLuint;
      v     : OpenGL.GLfloat_Matrix_4x4)
     with Convention => C;

   glVertexAttrib4fv : glewVertexAttrib4fv
     with Import, Convention => C, External_Name => "__glewVertexAttrib4fv";

   type glewUniform1i is access procedure
     (index : OpenGL.GLint;
      v0    : OpenGL.GLint)
     with Convention => C;

   glUniform1i : glewUniform1i
     with Import, Convention => C, External_Name => "__glewUniform1i";

   type glewUniform1f is access procedure
     (index : OpenGL.GLint;
      v0    : OpenGL.GLfloat)
     with Convention => C;

   glUniform1f : glewUniform1f
     with Import, Convention => C, External_Name => "__glewUniform1f";

   type glewUniform2f is access procedure
     (index : OpenGL.GLint;
      v0    : OpenGL.GLfloat;
      v1    : OpenGL.GLfloat)
     with Convention => C;

   glUniform2f : glewUniform2f
     with Import, Convention => C, External_Name => "__glewUniform2f";

   type glewUniform3f is access procedure
     (index : OpenGL.GLint;
      v0    : OpenGL.GLfloat;
      v1    : OpenGL.GLfloat;
      v2    : OpenGL.GLfloat)
     with Convention => C;

   glUniform3f : glewUniform3f
     with Import, Convention => C, External_Name => "__glewUniform3f";

   type glewUniform4f is access procedure
     (index : OpenGL.GLint;
      v0    : OpenGL.GLfloat;
      v1    : OpenGL.GLfloat;
      v2    : OpenGL.GLfloat;
      v4    : OpenGL.GLfloat)
     with Convention => C;

   glUniform4f : glewUniform4f
     with Import, Convention => C, External_Name => "__glewUniform4f";

   type glewUniformMatrix2fv is access procedure
     (index     : OpenGL.GLint;
      count     : OpenGL.GLsizei;
      transpose : OpenGL.GLint;
      v         : OpenGL.GLfloat_Matrix_2x2)
     with Convention => C;

   glUniformMatrix2fv : glewUniformMatrix2fv
     with Import, Convention => C, External_Name => "__glewUniformMatrix2fv";

   type glewUniformMatrix3fv is access procedure
     (index     : OpenGL.GLint;
      count     : OpenGL.GLsizei;
      transpose : OpenGL.GLint;
      v         : OpenGL.GLfloat_Matrix_3x3)
     with Convention => C;

   glUniformMatrix3fv : glewUniformMatrix3fv
     with Import, Convention => C, External_Name => "__glewUniformMatrix3fv";

   type glewUniformMatrix4fv is access procedure
     (index     : OpenGL.GLint;
      count     : OpenGL.GLsizei;
      transpose : OpenGL.GLint;
      v         : OpenGL.GLfloat_Matrix_4x4)
     with Convention => C;

   glUniformMatrix4fv : glewUniformMatrix4fv
     with Import, Convention => C, External_Name => "__glewUniformMatrix4fv";

   type glewGetUniformLocation is access function
     (program : GLuint;
      name    : Interfaces.C.char_array) return OpenGL.GLint
     with Convention => C;

   glGetUniformLocation : glewGetUniformLocation
     with Import, Convention => C, External_Name => "__glewGetUniformLocation";

   type glewActiveTexture is access procedure (texture : OpenGL.GLenum)
     with Convention => C;

   glActiveTexture : glewActiveTexture
     with Import, Convention => C, External_Name => "__glewActiveTexture";

   procedure glClear
     (mask : OpenGL.Clear_Buffer_Mask)
       with Import, Convention => C, External_Name => "glClear";

   procedure glDrawArrays
     (mode  : OpenGL.GLenum;
      first : OpenGL.GLint;
      count : OpenGL.GLsizei)
       with Import, Convention => C, External_Name => "glDrawArrays";

   procedure glDrawElements
     (mode   : OpenGL.GLenum;
      count  : OpenGL.GLsizei;
      tipe   : OpenGL.GLenum;
      offset : System.Address)
       with Import, Convention => C, External_Name => "glDrawElements";

   procedure glEnable(cap : OpenGL.GLenum)
     with Import, Convention => C, External_Name => "glEnable";

   procedure glDisable(cap : OpenGL.GLenum)
     with Import, Convention => C, External_Name => "glDisable";

   procedure glBlendFunc
     (sfactor : OpenGL.GLenum;
      dfactor : OpenGL.GLenum)
     with Import, Convention => C, External_Name => "glBlendFunc";

   procedure glClearColor
     (red, green, blue, alpha : OpenGL.GLclampf)
     with Import, Convention => C, External_Name => "glClearColor";

   procedure glClearDepth (depth : OpenGL.GLdouble)
     with Import, Convention => C, External_Name => "glClearDepth";

   procedure glDepthFunc (func : OpenGL.GLenum)
     with Import, Convention => C, External_Name => "glDepthFunc";

   procedure glFinish
     with Import, Convention => C, External_Name => "glFinish";

   procedure glFlush
     with Import, Convention => C, External_Name => "glFlush";

   procedure glViewport
     (x, y : OpenGL.GLint;
      width, height : OpenGL.GLsizei)
        with Import, Convention => C, External_Name => "glViewport";

   procedure glBindTexture
     (target  : OpenGL.GLenum;
      texture : GLuint)
        with Import, Convention => C, External_Name => "glBindTexture";

   procedure glGenTextures
     (n        : OpenGL.GLsizei;
      textures : access GLuint)
        with Import, Convention => C, External_Name => "glGenTextures";

   procedure glDeleteTextures
     (n        : OpenGL.GLsizei;
      textures : access GLuint)
        with Import, Convention => C, External_Name => "glDeleteTextures";

   procedure glTexImage2D
     (target         : OpenGL.GLenum;
      level          : OpenGL.GLint;
      internalformat : OpenGL.GLenum;
      width          : OpenGL.GLsizei;
      height         : OpenGL.GLsizei;
      border         : OpenGL.GLint;
      format         : OpenGL.GLenum;
      tipe           : OpenGL.GLenum;
      data           : System.Address)
        with Import, Convention => C, External_Name => "glTexImage2D";

   procedure glTexParameteri
     (target : OpenGL.GLenum;
      pname  : OpenGL.GLenum;
      param  : OpenGL.GLint)
        with Import, Convention => C, External_Name => "glTexParameteri";

   ARRAY_BUFFER         : constant := 16#8892#;
   ELEMENT_ARRAY_BUFFER : constant := 16#8893#;

   STATIC_DRAW : constant := 16#88E4#;

   FRAGMENT_SHADER : constant := 16#8B30#;
   VERTEX_SHADER   : constant := 16#8B31#;

   COMPILE_STATUS : constant := 16#8B81#;
   LINK_STATUS    : constant := 16#8B82#;

end GLEW;
