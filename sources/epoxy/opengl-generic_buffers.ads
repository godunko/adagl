--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System.Storage_Elements;

private with Gdk.GLContext;

private with epoxy;

generic
   type Element is private;
   type Index is (<>);
   type Element_Array is array (Index range <>) of Element;

package OpenGL.Generic_Buffers is

   type OpenGL_Buffer (Buffer_Type : OpenGL.Buffer_Type) is
     tagged limited private;

   procedure Allocate
    (Self : in out OpenGL_Buffer'Class; Data : Element_Array);
   --  Allocates necessary space to the buffer, initialized to the contents of
   --  Data. Any previous contents will be removed.

   function Bind (Self : in out OpenGL_Buffer'Class) return Boolean;
   --  Binds the buffer associated with this object to the current OpenGL
   --  context. Returns False if binding was not possible.
   --
   --  The buffer must be bound to the same OpenGL_Context current when Create
   --  was called. Otherwise, False will be returned from this function.

   procedure Bind (Self : in out OpenGL_Buffer'Class);
   --  Binds the buffer associated with this object to the current OpenGL
   --  context. Raise Program_Error if binding was not possible.
   --
   --  The buffer must be bound to the same OpenGL_Context current when Create
   --  was called. Otherwise, Program_Error will be raised by this function.

   function Create (Self : in out OpenGL_Buffer'Class) return Boolean;
   --  Creates the buffer object in the OpenGL server. Returns True if the
   --  object was created; False otherwise.

   procedure Create (Self : in out OpenGL_Buffer'Class);
   --  Creates the buffer object in the OpenGL server. Raise Program_Error if
   --  the object was created; False otherwise.

   function Stride return System.Storage_Elements.Storage_Count;
   --  Returns offset between two elements in element array in storage units.

private

   type OpenGL_Buffer (Buffer_Type : OpenGL.Buffer_Type) is
     tagged limited record
      Context : Gdk.GLContext.Gdk_GLContext;
      Buffer  : aliased epoxy.GLuint := 0;
   end record;

end OpenGL.Generic_Buffers;
