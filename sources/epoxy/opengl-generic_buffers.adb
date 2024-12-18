--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces.C;

with epoxy_gl_generated_h;
with OpenGL.Contexts.Internals;

package body OpenGL.Generic_Buffers is

   Map : constant array (OpenGL.Buffer_Type) of OpenGL.GLenum :=
     (OpenGL.Vertex => epoxy_gl_generated_h.GL_ARRAY_BUFFER,
      OpenGL.Index  => epoxy_gl_generated_h.GL_ELEMENT_ARRAY_BUFFER);

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Self : in out OpenGL_Buffer'Class; Data : Element_Array)
   is
      use type Interfaces.C.ptrdiff_t;

   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context) then
         --  Buffer was not created or created for another context.

         return;
      end if;

      epoxy_gl_generated_h.glBufferData
        (target => Map (Self.Buffer_Type),
         size   => Data'Size / 8,
         data   => Data'Address,
         usage  => epoxy_gl_generated_h.GL_STATIC_DRAW);
   end Allocate;

   ----------
   -- Bind --
   ----------

   function Bind (Self : in out OpenGL_Buffer'Class) return Boolean is
   begin
      if not OpenGL.Contexts.Internals.Is_Current (Self.Context) then
         --  Buffer was not created or created for another context.

         return False;
      end if;

      epoxy_gl_generated_h.glBindBuffer
        (target => Map (Self.Buffer_Type),
         buffer => Self.Buffer);

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
      use type Interfaces.C.unsigned;
      use type Gdk.GLContext.Gdk_GLContext;
      use type epoxy.GLuint;

   begin
      if Self.Context = null then
         Self.Context := OpenGL.Contexts.Internals.Current_Gdk_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Buffer = 0 then
         epoxy_gl_generated_h.glGenBuffers (1, Self.Buffer'Unchecked_Access);

         if Self.Buffer = 0 then
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
