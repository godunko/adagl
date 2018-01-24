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

package body OpenGL.Generic_Buffers is

   use type WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access;
   use type WebAPI.WebGL.Buffers.WebGL_Buffer_Access;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
    (Self : in out OpenGL_Buffer'Class; Data : Element_Array) is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
      then
         --  Buffer was not created or created for another context.

         return;
      end if;

      Self.Context.Buffer_Data
       (WebAPI.WebGL.Rendering_Contexts.ARRAY_BUFFER,
        Data'Address,
        WebAPI.WebGL.Rendering_Contexts.STATIC_DRAW);
   end Allocate;

   ----------
   -- Bind --
   ----------

   function Bind (Self : in out OpenGL_Buffer'Class) return Boolean is
   begin
      if Self.Context = null
        or Self.Context /= OpenGL.Contexts.Internals.Current_WebGL_Context
      then
         --  Buffer was not created or created for another context.

         return False;
      end if;

      Self.Context.Bind_Buffer
       (WebAPI.WebGL.Rendering_Contexts.ARRAY_BUFFER, Self.Buffer);

      return True;
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind (Self : in out OpenGL_Buffer'Class) is
   begin
      if not Self.Bind then
         raise Program_Error;
      end if;
   end Bind;

   ------------
   -- Create --
   ------------

   function Create (Self : in out OpenGL_Buffer'Class) return Boolean is
   begin
      if Self.Context = null then
         Self.Context := OpenGL.Contexts.Internals.Current_WebGL_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Buffer = null then
         Self.Buffer := Self.Context.Create_Buffer;

         if Self.Buffer = null then
            Self.Context := null;

            return False;
         end if;
      end if;

      return True;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create (Self : in out OpenGL_Buffer'Class) is
   begin
      if not Self.Create then
         raise Program_Error;
      end if;
   end Create;

   ------------
   -- Stride --
   ------------

   function Stride return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Offset;

   begin
      return Element_Array'Component_Size / System.Storage_Unit;
   end Stride;

end OpenGL.Generic_Buffers;
