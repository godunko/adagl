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

package body OpenGL.Renderbuffers is

   use type WebAPI.WebGL.Renderbuffers.WebGL_Renderbuffer_Access;
   use type WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access;

   ----------
   -- Bind --
   ----------

   function Bind (Self : in out OpenGL_Renderbuffer'Class) return Boolean is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Renderbuffer = null
      then
         return False;
      end if;

      Self.Context.Bind_Renderbuffer
       (WebAPI.WebGL.Rendering_Contexts.RENDERBUFFER, Self.Renderbuffer);

      return True;
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind (Self : in out OpenGL_Renderbuffer'Class) is
   begin
      if not Self.Bind then
         raise Program_Error;
      end if;
   end Bind;

   ------------
   -- Create --
   ------------

   function Create (Self : in out OpenGL_Renderbuffer'Class) return Boolean is
   begin
      if Self.Context = null then
         Self.Context := OpenGL.Contexts.Internals.Current_WebGL_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Renderbuffer = null then
         Self.Renderbuffer := Self.Context.Create_Renderbuffer;

         if Self.Renderbuffer = null then
            return False;
         end if;
      end if;

      return True;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (Self : in out OpenGL_Renderbuffer'Class) is
   begin
      if not Self.Create then
         raise Program_Error;
      end if;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out OpenGL_Renderbuffer'Class) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Renderbuffer = null
      then
         return;
      end if;

      Self.Context.Delete_Renderbuffer (Self.Renderbuffer);

      Self.Renderbuffer := null;
      Self.Context      := null;
   end Delete;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out OpenGL_Renderbuffer'Class) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Renderbuffer = null
      then
         return;
      end if;

      Self.Context.Bind_Renderbuffer
       (WebAPI.WebGL.Rendering_Contexts.RENDERBUFFER, null);
   end Release;

   -------------
   -- Storage --
   -------------

   procedure Storage
    (Self   : in out OpenGL_Renderbuffer'Class;
     Format : OpenGL.GLenum;
     Width  : OpenGL.GLsizei;
     Height : OpenGL.GLsizei) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
        or Self.Renderbuffer = null
      then
         return;
      end if;

      Self.Context.Renderbuffer_Storage
       (WebAPI.WebGL.Rendering_Contexts.RENDERBUFFER,
        (case Format is
           when GL_RGBA4 =>
             WebAPI.WebGL.Rendering_Contexts.RGBA4,
           when GL_RGB565 =>
             WebAPI.WebGL.Rendering_Contexts.RGB565,
           when GL_RGB5_A1 =>
             WebAPI.WebGL.Rendering_Contexts.RGB5_A1,
           when GL_DEPTH_COMPONENT16 =>
             WebAPI.WebGL.Rendering_Contexts.DEPTH_COMPONENT16,
           when GL_STENCIL_INDEX8 =>
             WebAPI.WebGL.Rendering_Contexts.STENCIL_INDEX8,
           when others =>
             WebAPI.WebGL.Rendering_Contexts.RGBA4),
--             raise Constraint_Error),
        WebAPI.WebGL.GLsizei (Width),
        WebAPI.WebGL.GLsizei (Height));
   end Storage;

end OpenGL.Renderbuffers;
