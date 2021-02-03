------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2016-2021, Vadim Godunko <vgodunko@gmail.com>                --
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
--with OpenGL.Renderbuffers.Internals;
--with OpenGL.Textures.Internals;

package body OpenGL.Framebuffers is

   use type Web.GL.Rendering_Contexts.WebGL_Rendering_Context;

--   use type WebAPI.WebGL.Framebuffers.WebGL_Framebuffer_Access;
--
--   ----------
--   -- Bind --
--   ----------
--
--   function Bind (Self : in out OpenGL_Framebuffer'Class) return Boolean is
--   begin
--      if Self.Context = null
--        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
--        or Self.Framebuffer = null
--      then
--         return False;
--      end if;
--
--      Self.Context.Bind_Framebuffer
--       (WebAPI.WebGL.Rendering_Contexts.FRAMEBUFFER, Self.Framebuffer);
--
--      return True;
--   end Bind;
--
--   ----------
--   -- Bind --
--   ----------
--
--   procedure Bind (Self : in out OpenGL_Framebuffer'Class) is
--   begin
--      if not Self.Bind then
--         raise Program_Error;
--      end if;
--   end Bind;

   ------------
   -- Create --
   ------------

   function Create
     (Self   : in out OpenGL_Framebuffer'Class;
      Width  : Natural;
      Height : Natural) return Boolean is
   begin
      if Self.Context.Is_Null then
         Self.Context := OpenGL.Contexts.Internals.Current_WebGL_Context;

         if Self.Context.Is_Null then
            return False;
         end if;
      end if;

      if Self.Framebuffer.Is_Null then
         Self.Framebuffer := Self.Context.Create_Framebuffer;

         if Self.Framebuffer.Is_Null then
            return False;
         end if;
      end if;

      Self.Context.Bind_Framebuffer
       (Web.GL.Rendering_Contexts.FRAMEBUFFER, Self.Framebuffer);

      --  Initialize texture.

      if Self.Texture.Is_Null then
         Self.Texture := Self.Context.Create_Texture;

         if Self.Texture.Is_Null then
            return False;
         end if;
      end if;

      Self.Context.Bind_Texture
        (Web.GL.Rendering_Contexts.TEXTURE_2D, Self.Texture);

      Self.Context.Tex_Parameteri
        (Web.GL.Rendering_Contexts.TEXTURE_2D,
         Web.GL.Rendering_Contexts.TEXTURE_MIN_FILTER,
         Web.GL.Rendering_Contexts.NEAREST);
      Self.Context.Tex_Parameteri
        (Web.GL.Rendering_Contexts.TEXTURE_2D,
         Web.GL.Rendering_Contexts.TEXTURE_MAG_FILTER,
         Web.GL.Rendering_Contexts.NEAREST);
      Self.Context.Tex_Parameteri
        (Web.GL.Rendering_Contexts.TEXTURE_2D,
         Web.GL.Rendering_Contexts.TEXTURE_WRAP_S,
         Web.GL.Rendering_Contexts.CLAMP_TO_EDGE);
      Self.Context.Tex_Parameteri
        (Web.GL.Rendering_Contexts.TEXTURE_2D,
         Web.GL.Rendering_Contexts.TEXTURE_WRAP_T,
         Web.GL.Rendering_Contexts.CLAMP_TO_EDGE);

      Self.Context.Tex_Image_2D
        (Web.GL.Rendering_Contexts.TEXTURE_2D,
         0,
         Web.GL.Rendering_Contexts.RGBA,
         Web.GL.GLsizei (Width),
         Web.GL.GLsizei (Height),
         0,
         Web.GL.Rendering_Contexts.RGBA,
         Web.GL.Rendering_Contexts.UNSIGNED_BYTE);
      Self.Context.Framebuffer_Texture_2D
        (Web.GL.Rendering_Contexts.FRAMEBUFFER,
         Web.GL.Rendering_Contexts.COLOR_ATTACHMENT_0,
         Web.GL.Rendering_Contexts.TEXTURE_2D,
         Self.Texture,
         0);

      --  unbind texture?

      --  Initialize renderbuffer.

      if Self.Renderbuffer.Is_Null then
         Self.Renderbuffer := Self.Context.Create_Renderbuffer;

         if Self.Renderbuffer.Is_Null then
            return False;
         end if;
      end if;

      Self.Context.Bind_Renderbuffer
        (Web.GL.Rendering_Contexts.RENDERBUFFER, Self.Renderbuffer);

      Self.Context.Renderbuffer_Storage
        (Web.GL.Rendering_Contexts.RENDERBUFFER,
         Web.GL.Rendering_Contexts.DEPTH_COMPONENT16,
--         Web.GL.Rendering_Contexts.DEPTH_STENCIL,
         Web.GL.Glsizei (Width),
         Web.GL.Glsizei (Height));
      Self.Context.Framebuffer_Renderbuffer
        (Web.GL.Rendering_Contexts.FRAMEBUFFER,
         Web.GL.Rendering_Contexts.DEPTH_ATTACHMENT,
--         Web.GL.Rendering_Contexts.DEPTH_STENCIL_ATTACHMENT,
         Web.GL.Rendering_Contexts.RENDERBUFFER,
         Self.Renderbuffer);

      --  unbind renderbuffer?

      --  unbind framebuffer?

      return True;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self   : in out OpenGL_Framebuffer'Class;
      Width  : Natural;
      Height : Natural) is
   begin
      if not Self.Create (Width, Height) then
         raise Program_Error;
      end if;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out OpenGL_Framebuffer'Class) is
   begin
      if Self.Context.Is_Null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
      then
         return;
      end if;

      if not Self.Framebuffer.Is_Null then
         Self.Context.Delete_Framebuffer (Self.Framebuffer);
      end if;

      if not Self.Texture.Is_Null then
         Self.Context.Delete_Texture (Self.Texture);
      end if;

      Self.Texture.Set_Null;
      Self.Framebuffer.Set_Null;
      Self.Context.Set_Null;
   end Delete;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out OpenGL_Framebuffer) is
   begin
      Self.Delete;
   end Finalize;

   -----------------
   -- Read_Pixels --
   -----------------

   procedure Read_Pixels
    (Self   : in out OpenGL_Framebuffer'Class;
     X      : OpenGL.GLint;
     Y      : OpenGL.GLint;
     Width  : OpenGL.GLsizei;
     Height : OpenGL.GLsizei;
     Pixels : System.Address;
     Size   : Interfaces.Unsigned_32) is
--   procedure Read_Pixels
--    (Self   : in out OpenGL_Framebuffer'Class;
--     X      : OpenGL.GLint;
--     Y      : OpenGL.GLint;
--     Width  : OpenGL.GLsizei;
--     Height : OpenGL.GLsizei;
--     Data   : out OpenGL.GLubyte_Vector_4_Array) is
   begin
      if Self.Context.Is_Null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Framebuffer.Is_Null
      then
         return;
      end if;

      Self.Context.Read_Pixels
       (Web.GL.GLint (X),
        Web.GL.GLint (Y),
        Web.GL.GLsizei (Width),
        Web.GL.GLsizei (Height),
        Web.GL.Rendering_Contexts.RGBA,
        Web.GL.Rendering_Contexts.UNSIGNED_BYTE,
        Pixels,
        Size);
--        Data'Address);
   end Read_Pixels;

--   -------------
--   -- Release --
--   -------------
--
--   procedure Release (Self : in out OpenGL_Framebuffer'Class) is
--   begin
--      if Self.Context = null
--        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
--        or Self.Framebuffer = null
--      then
--         return;
--      end if;
--
--      Self.Context.Bind_Framebuffer
--       (WebAPI.WebGL.Rendering_Contexts.FRAMEBUFFER, null);
--   end Release;
--
--   ----------------------
--   -- Set_Renderbuffer --
--   ----------------------
--
--   procedure Set_Renderbuffer
--    (Self         : in out OpenGL_Framebuffer'Class;
--     Renderbuffer : OpenGL.Renderbuffers.OpenGL_Renderbuffer'Class;
--     Attachment   : OpenGL.GLenum) is
--   begin
--      if Self.Context = null
--        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
--        or Self.Framebuffer = null
--      then
--         return;
--      end if;
--
--      Self.Context.Framebuffer_Renderbuffer
--       (WebAPI.WebGL.Rendering_Contexts.FRAMEBUFFER,
--        (case Attachment is
--           when OpenGL.GL_COLOR_ATTACHMENT0  =>
--             WebAPI.WebGL.Rendering_Contexts.COLOR_ATTACHMENT0,
--           when OpenGL.GL_DEPTH_ATTACHMENT   =>
--             WebAPI.WebGL.Rendering_Contexts.DEPTH_ATTACHMENT,
--           when OpenGL.GL_STENCIL_ATTACHMENT =>
--             WebAPI.WebGL.Rendering_Contexts.STENCIL_ATTACHMENT,
--           when others =>
--             WebAPI.WebGL.Rendering_Contexts.COLOR_ATTACHMENT0),
----             raise Constraint_Error),
--        WebAPI.WebGL.Rendering_Contexts.RENDERBUFFER,
--        OpenGL.Renderbuffers.Internals.Get_WebGL_Renderbuffer (Renderbuffer));
--   end Set_Renderbuffer;
--
--   -----------------
--   -- Set_Texture --
--   -----------------
--
--   procedure Set_Texture
--    (Self       : in out OpenGL_Framebuffer'Class;
--     Texture    : OpenGL.Textures.OpenGL_Texture'Class;
--     Attachment : OpenGL.GLenum) is
--   begin
--      if Self.Context = null
--        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
--        or Self.Framebuffer = null
--      then
--         return;
--      end if;
--
--      Self.Context.Framebuffer_Texture_2D
--       (WebAPI.WebGL.Rendering_Contexts.FRAMEBUFFER,
--        (case Attachment is
--           when OpenGL.GL_COLOR_ATTACHMENT0  =>
--             WebAPI.WebGL.Rendering_Contexts.COLOR_ATTACHMENT0,
--           when OpenGL.GL_DEPTH_ATTACHMENT   =>
--             WebAPI.WebGL.Rendering_Contexts.DEPTH_ATTACHMENT,
--           when OpenGL.GL_STENCIL_ATTACHMENT =>
--             WebAPI.WebGL.Rendering_Contexts.STENCIL_ATTACHMENT,
--           when others =>
--             WebAPI.WebGL.Rendering_Contexts.COLOR_ATTACHMENT0),
----             raise Constraint_Error),
--        (case Texture.Texture_Type is
--           when Texture_2D =>
--             WebAPI.WebGL.Rendering_Contexts.TEXTURE_2D,
--           when Cube_Map_Positive_X =>
--             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_POSITIVE_X,
--           when Cube_Map_Negative_X =>
--             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_NEGATIVE_X,
--           when Cube_Map_Positive_Y =>
--             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_POSITIVE_Y,
--           when Cube_Map_Negative_Y =>
--             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_NEGATIVE_Y,
--           when Cube_Map_Positive_Z =>
--             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_POSITIVE_Z,
--           when Cube_Map_Negative_Z =>
--             WebAPI.WebGL.Rendering_Contexts.TEXTURE_CUBE_MAP_NEGATIVE_Z),
--        OpenGL.Textures.Internals.Get_WebGL_Texture (Texture),
--        0);
--   end Set_Texture;

end OpenGL.Framebuffers;
