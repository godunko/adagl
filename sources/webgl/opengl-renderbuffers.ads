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
private with WebAPI.WebGL.Renderbuffers;
private with WebAPI.WebGL.Rendering_Contexts;

package OpenGL.Renderbuffers is

   pragma Preelaborate;

   type OpenGL_Renderbuffer is tagged limited private;

   function Create (Self : in out OpenGL_Renderbuffer'Class) return Boolean;
   procedure Create (Self : in out OpenGL_Renderbuffer'Class);

   procedure Delete (Self : in out OpenGL_Renderbuffer'Class);

   function Bind (Self : in out OpenGL_Renderbuffer'Class) return Boolean;
   procedure Bind (Self : in out OpenGL_Renderbuffer'Class);

   procedure Release (Self : in out OpenGL_Renderbuffer'Class);

   procedure Storage
    (Self   : in out OpenGL_Renderbuffer'Class;
     Format : OpenGL.GLenum;
     Width  : OpenGL.GLsizei;
     Height : OpenGL.GLsizei);

private

   type OpenGL_Renderbuffer is tagged limited record
      Renderbuffer : WebAPI.WebGL.Renderbuffers.WebGL_Renderbuffer_Access;
      Context      :
        WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access;
   end record;

end OpenGL.Renderbuffers;
