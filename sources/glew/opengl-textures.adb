------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2018, Vadim Godunko <vgodunko@gmail.com>                     --
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

with System;

with GLEW; use GLEW;

with OpenGL.Contexts.Internals;
with League.Stream_Element_Vectors.Internals;

package body OpenGL.Textures is

   use type GLFW.GLFWwindow_Access;

   Map : constant array (OpenGL.Texture_Type) of OpenGL.GLenum :=
     (Texture_2D          => 16#0DE1#,
      Cube_Map_Positive_X => 16#8515#,
      Cube_Map_Negative_X => 16#8516#,
      Cube_Map_Positive_Y => 16#8517#,
      Cube_Map_Negative_Y => 16#8518#,
      Cube_Map_Positive_Z => 16#8519#,
      Cube_Map_Negative_Z => 16#851A#);

   Map_Bind : constant array (OpenGL.Texture_Type) of OpenGL.GLenum :=
     (Texture_2D          => 16#0DE1#,
      Cube_Map_Positive_X => 16#8513#,
      Cube_Map_Negative_X => 16#8513#,
      Cube_Map_Positive_Y => 16#8513#,
      Cube_Map_Negative_Y => 16#8513#,
      Cube_Map_Positive_Z => 16#8513#,
      Cube_Map_Negative_Z => 16#8513#);

   procedure Load_TGA
     (Name : League.Strings.Universal_String;
      Data : out Image_Data) is separate;

   ----------
   -- Bind --
   ----------

   function Bind (Self : in out OpenGL_Texture'Class) return Boolean is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Self.Texture = 0
      then
         return False;
      end if;

      GLEW.glBindTexture (Map_Bind (Self.Texture_Type), Self.Texture);

      return True;
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind (Self : in out OpenGL_Texture'Class) is
   begin
      if not Self.Bind then
         raise Program_Error;
      end if;
   end Bind;

   ----------
   -- Bind --
   ----------

   function Bind
    (Self : in out OpenGL_Texture'Class;
     Unit : Texture_Unit) return Boolean is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Self.Texture = 0
      then
         return False;
      end if;

      GLEW.glActiveTexture (16#84C0# + GLenum (Unit));  --  GL_TEXTURE0
      GLEW.glBindTexture (Map_Bind (Self.Texture_Type), Self.Texture);

      return True;
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind
    (Self : in out OpenGL_Texture'Class;
     Unit : Texture_Unit) is
   begin
      if not Self.Bind (Unit) then
         raise Program_Error;
      end if;
   end Bind;

   ------------
   -- Create --
   ------------

   function Create (Self : in out OpenGL_Texture'Class) return Boolean is
   begin
      if Self.Context = null then
         Self.Context := OpenGL.Contexts.Internals.Current_GLFW_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Texture = 0 then
         GLEW.glGenTextures (1, Self.Texture'Unchecked_Access);

         if Self.Texture = 0 then
            return False;
         end if;
      end if;

      return True;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (Self : in out OpenGL_Texture'Class) is
   begin
      if not Self.Create then
         raise Program_Error;
      end if;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out OpenGL_Texture'Class) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Self.Texture = 0
      then
         return;
      end if;

      GLEW.glDeleteTextures (1, Self.Texture'Unchecked_Access);

      Self.Texture := 0;
      Self.Context := null;
   end Delete;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
    (Self   : in out OpenGL_Texture'Class;
     Name   : League.Strings.Universal_String;
     Level  : OpenGL.GLint := 0;
     Format : OpenGL.GLenum := OpenGL.GL_RGB) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Self.Texture = 0
      then
         return;
      elsif Name.To_Lowercase.Ends_With (".tga") then
         Load_TGA (Name, Self.Data);
      else
         raise Constraint_Error with "Unknown format";
      end if;

      GLEW.glTexImage2D
        (Map (Self.Texture_Type), 0, Format,
         Self.Data.Width, Self.Data.Height, 0,
         Self.Data.Format, Self.Data.Data_Type,
         League.Stream_Element_Vectors.Internals.Internal
           (Self.Data.Raw).Value (0)'Address);
   end Load_File;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out OpenGL_Texture'Class) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Self.Texture = 0
      then
         return;
      end if;

      GLEW.glBindTexture (Map_Bind (Self.Texture_Type), 0);
   end Release;

   ------------------
   -- Set_Image_2D --
   ------------------

   procedure Set_Image_2D
    (Self      : in out OpenGL_Texture'Class;
     Level     : OpenGL.GLint;
     Format    : OpenGL.GLenum;
     Width     : OpenGL.GLsizei;
     Height    : OpenGL.GLsizei;
     Data_Type : OpenGL.GLenum;
     Data      : System.Address)
   is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Self.Texture = 0
      then
         return;
      end if;

      GLEW.glTexImage2D
        (Map (Self.Texture_Type), Level, Format, Width, Height, 0,
         Format, Data_Type, Data);
   end Set_Image_2D;

   -------------------
   -- Set_Parameter --
   -------------------

   procedure Set_Parameter
    (Self      : in out OpenGL_Texture'Class;
     Parameter : OpenGL.GLenum;
     Value     : OpenGL.GLenum) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_GLFW_Context
        or Self.Texture = 0
      then
         return;
      end if;

      GLEW.glTexParameteri
        (Map_Bind (Self.Texture_Type), Parameter, OpenGL.GLint (Value));
   end Set_Parameter;

end OpenGL.Textures;
