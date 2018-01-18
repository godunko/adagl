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
with OpenGL.Contexts.Internals;
with OpenGL.Renderbuffers.Internals;
with OpenGL.Textures.Internals;

package body OpenGL.Framebuffers is

   use type WebAPI.WebGL.Framebuffers.WebGL_Framebuffer_Access;
   use type WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access;

   ----------
   -- Bind --
   ----------

   function Bind (Self : in out OpenGL_Framebuffer'Class) return Boolean is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Framebuffer = null
      then
         return False;
      end if;

      Self.Context.Bind_Framebuffer
       (WebAPI.WebGL.Rendering_Contexts.FRAMEBUFFER, Self.Framebuffer);

      return True;
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind (Self : in out OpenGL_Framebuffer'Class) is
   begin
      if not Self.Bind then
         raise Program_Error;
      end if;
   end Bind;

   ------------
   -- Create --
   ------------

   function Create (Self : in out OpenGL_Framebuffer'Class) return Boolean is
   begin
      if Self.Context = null then
         Self.Context := OpenGL.Contexts.Internals.Current_WebGL_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Framebuffer = null then
         Self.Framebuffer := Self.Context.Create_Framebuffer;

         if Self.Framebuffer = null then
            return False;
         end if;
      end if;

      return True;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (Self : in out OpenGL_Framebuffer'Class) is
   begin
      if not Self.Create then
         raise Program_Error;
      end if;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out OpenGL_Framebuffer'Class) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Framebuffer = null
      then
         return;
      end if;

      Self.Context.Delete_Framebuffer (Self.Framebuffer);

      Self.Framebuffer := null;
      Self.Context     := null;
   end Delete;

   -----------------
   -- Read_Pixels --
   -----------------

   procedure Read_Pixels
    (Self   : in out OpenGL_Framebuffer'Class;
     X      : OpenGL.GLint;
     Y      : OpenGL.GLint;
     Width  : OpenGL.GLsizei;
     Height : OpenGL.GLsizei;
     Data   : out OpenGL.GLubyte_Vector_4_Array) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Framebuffer = null
      then
         return;
      end if;

      Self.Context.Read_Pixels
       (WebAPI.WebGL.GLint (X),
        WebAPI.WebGL.GLint (Y),
        WebAPI.WebGL.GLsizei (Width),
        WebAPI.WebGL.GLsizei (Height),
        WebAPI.WebGL.Rendering_Contexts.RGBA,
        WebAPI.WebGL.Rendering_Contexts.UNSIGNED_BYTE,
        Data'Address);
   end Read_Pixels;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out OpenGL_Framebuffer'Class) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Framebuffer = null
      then
         return;
      end if;

      Self.Context.Bind_Framebuffer
       (WebAPI.WebGL.Rendering_Contexts.FRAMEBUFFER, null);
   end Release;

   ----------------------
   -- Set_Renderbuffer --
   ----------------------

   procedure Set_Renderbuffer
    (Self         : in out OpenGL_Framebuffer'Class;
     Renderbuffer : OpenGL.Renderbuffers.OpenGL_Renderbuffer'Class;
     Attachment   : OpenGL.GLenum) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Framebuffer = null
      then
         return;
      end if;

      Self.Context.Framebuffer_Renderbuffer
       (WebAPI.WebGL.Rendering_Contexts.FRAMEBUFFER,
        (case Attachment is
           when OpenGL.GL_COLOR_ATTACHMENT0  =>
             WebAPI.WebGL.Rendering_Contexts.COLOR_ATTACHMENT0,
           when OpenGL.GL_DEPTH_ATTACHMENT   =>
             WebAPI.WebGL.Rendering_Contexts.DEPTH_ATTACHMENT,
           when OpenGL.GL_STENCIL_ATTACHMENT =>
             WebAPI.WebGL.Rendering_Contexts.STENCIL_ATTACHMENT,
           when others =>
             WebAPI.WebGL.Rendering_Contexts.COLOR_ATTACHMENT0),
--             raise Constraint_Error),
        WebAPI.WebGL.Rendering_Contexts.RENDERBUFFER,
        OpenGL.Renderbuffers.Internals.Get_WebGL_Renderbuffer (Renderbuffer));
   end Set_Renderbuffer;

   -----------------
   -- Set_Texture --
   -----------------

   procedure Set_Texture
    (Self       : in out OpenGL_Framebuffer'Class;
     Texture    : OpenGL.Textures.OpenGL_Texture'Class;
     Attachment : OpenGL.GLenum) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Framebuffer = null
      then
         return;
      end if;

      Self.Context.Framebuffer_Texture_2D
       (WebAPI.WebGL.Rendering_Contexts.FRAMEBUFFER,
        (case Attachment is
           when OpenGL.GL_COLOR_ATTACHMENT0  =>
             WebAPI.WebGL.Rendering_Contexts.COLOR_ATTACHMENT0,
           when OpenGL.GL_DEPTH_ATTACHMENT   =>
             WebAPI.WebGL.Rendering_Contexts.DEPTH_ATTACHMENT,
           when OpenGL.GL_STENCIL_ATTACHMENT =>
             WebAPI.WebGL.Rendering_Contexts.STENCIL_ATTACHMENT,
           when others =>
             WebAPI.WebGL.Rendering_Contexts.COLOR_ATTACHMENT0),
--             raise Constraint_Error),
        (case Texture.Texture_Type is
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
        OpenGL.Textures.Internals.Get_WebGL_Texture (Texture),
        0);
   end Set_Texture;

end OpenGL.Framebuffers;
