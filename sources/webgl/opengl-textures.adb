------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2016-2018, Vadim Godunko <vgodunko@gmail.com>                --
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

with OpenGL.Contexts.Internals;

package body OpenGL.Textures is

   use type WebAPI.WebGL.Textures.WebGL_Texture_Access;
   use type WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access;

   ----------
   -- Bind --
   ----------

   function Bind (Self : in out OpenGL_Texture'Class) return Boolean is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Texture = null
      then
         return False;
      end if;

      Self.Context.Bind_Texture
       ((case Self.Texture_Type is
           when Texture_2D => WebAPI.WebGL.Rendering_Contexts.TEXTURE_2D,
           when Cube_Map_Positive_X
                 | Cube_Map_Negative_X
                 | Cube_Map_Positive_Y
                 | Cube_Map_Negative_Y
                 | Cube_Map_Positive_Z
                 | Cube_Map_Negative_Z =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP),
        Self.Texture);

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
     Unit : Texture_Unit) return Boolean
   is
      use type WebAPI.WebGL.GLenum;

   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Texture = null
      then
         return False;
      end if;

      Self.Context.Active_Texture
       (WebAPI.WebGL.Rendering_Contexts.TEXTURE0 + WebAPI.WebGL.GLenum (Unit));
      Self.Context.Bind_Texture
       ((case Self.Texture_Type is
           when Texture_2D => WebAPI.WebGL.Rendering_Contexts.TEXTURE_2D,
           when Cube_Map_Positive_X
                 | Cube_Map_Negative_X
                 | Cube_Map_Positive_Y
                 | Cube_Map_Negative_Y
                 | Cube_Map_Positive_Z
                 | Cube_Map_Negative_Z =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP),
        Self.Texture);

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
         Self.Context := OpenGL.Contexts.Internals.Current_WebGL_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Texture = null then
         Self.Texture := Self.Context.Create_Texture;

         if Self.Texture = null then
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Texture = null
      then
         return;
      end if;

      Self.Context.Delete_Texture (Self.Texture);

      Self.Texture := null;
      Self.Context := null;
   end Delete;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out OpenGL_Texture'Class) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Texture = null
      then
         return;
      end if;

      Self.Context.Bind_Texture
       ((case Self.Texture_Type is
           when Texture_2D => WebAPI.WebGL.Rendering_Contexts.TEXTURE_2D,
           when Cube_Map_Positive_X
                 | Cube_Map_Negative_X
                 | Cube_Map_Positive_Y
                 | Cube_Map_Negative_Y
                 | Cube_Map_Positive_Z
                 | Cube_Map_Negative_Z =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP),
        null);
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
     Data_Type : OpenGL.GLenum) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Texture = null
      then
         return;
      end if;

      Self.Context.Tex_Image_2D
       ((case Self.Texture_Type is
           when Texture_2D =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_2D,
           when Cube_Map_Positive_X =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_POSITIVE_X,
           when Cube_Map_Negative_X =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_NEGATIVE_X,
           when Cube_Map_Positive_Y =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_POSITIVE_Y,
           when Cube_Map_Negative_Y =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_NEGATIVE_Y,
           when Cube_Map_Positive_Z =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_POSITIVE_Z,
           when Cube_Map_Negative_Z =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_NEGATIVE_Z),
        WebAPI.WebGL.GLint (Level),
        (case Format is
           when GL_ALPHA =>
             WebAPI.WebGL.Rendering_Contexts.ALPHA,
           when GL_LUMINANCE =>
             WebAPI.WebGL.Rendering_Contexts.LUMINANCE,
           when GL_LUMINANCE_ALPHA =>
             WebAPI.WebGL.Rendering_Contexts.LUMINANCE_ALPHA,
           when GL_RGB =>
             WebAPI.WebGL.Rendering_Contexts.RGB,
           when GL_RGBA =>
             WebAPI.WebGL.Rendering_Contexts.RGBA,
           when others =>
             WebAPI.WebGL.Rendering_Contexts.RGBA),
        WebAPI.WebGL.GLsizei (Width),
        WebAPI.WebGL.GLsizei (Height),
        0,
        (case Format is
           when GL_ALPHA =>
             WebAPI.WebGL.Rendering_Contexts.ALPHA,
           when GL_LUMINANCE =>
             WebAPI.WebGL.Rendering_Contexts.LUMINANCE,
           when GL_LUMINANCE_ALPHA =>
             WebAPI.WebGL.Rendering_Contexts.LUMINANCE_ALPHA,
           when GL_RGB =>
             WebAPI.WebGL.Rendering_Contexts.RGB,
           when GL_RGBA =>
             WebAPI.WebGL.Rendering_Contexts.RGBA,
           when others =>
             WebAPI.WebGL.Rendering_Contexts.RGBA),
        (case Data_Type is
           when GL_UNSIGNED_BYTE =>
             WebAPI.WebGL.Rendering_Contexts.UNSIGNED_BYTE,
           when GL_UNSIGNED_SHORT_5_6_5 =>
             WebAPI.WebGL.Rendering_Contexts.UNSIGNED_SHORT_5_6_5,
           when GL_UNSIGNED_SHORT_4_4_4_4 =>
             WebAPI.WebGL.Rendering_Contexts.UNSIGNED_SHORT_4_4_4_4,
           when GL_UNSIGNED_SHORT_5_5_5_1 =>
             WebAPI.WebGL.Rendering_Contexts.UNSIGNED_SHORT_5_5_5_1,
           when others =>
             WebAPI.WebGL.Rendering_Contexts.UNSIGNED_BYTE),
        System.Null_Address);
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
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Texture = null
      then
         return;
      end if;

      Self.Context.Tex_Parameteri
       ((case Self.Texture_Type is
           when Texture_2D => WebAPI.WebGL.Rendering_Contexts.TEXTURE_2D,
           when Cube_Map_Positive_X
                 | Cube_Map_Negative_X
                 | Cube_Map_Positive_Y
                 | Cube_Map_Negative_Y
                 | Cube_Map_Positive_Z
                 | Cube_Map_Negative_Z =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP),
        (case Parameter is
           when OpenGL.GL_TEXTURE_MIN_FILTER =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_MIN_FILTER,
           when OpenGL.GL_TEXTURE_MAG_FILTER =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_MAG_FILTER,
           when OpenGL.GL_TEXTURE_WRAP_S =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_WRAP_S,
           when OpenGL.GL_TEXTURE_WRAP_T =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_WRAP_T,
           when others =>
             WebAPI.WebGL.Rendering_Contexts.TEXTURE_MIN_FILTER),
--             raise Constraint_Error),
        WebAPI.WebGL.GLint (Value));
   end Set_Parameter;

end OpenGL.Textures;
