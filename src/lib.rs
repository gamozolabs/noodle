extern crate core;

use core::convert::TryInto;

trait Serialize {
    fn serialize(&self, buf: &mut Vec<u8>);
}

trait Deserialize: Sized {
    fn deserialize(ptr: &mut &[u8]) -> Option<Self>;
}

/// Implement `Serialize` trait for types which provide `to_le_bytes()`
macro_rules! serialize_le {
    // Serialize `$input_type` as an `$wire_type` by using `to_le_bytes()`
    // and `from_le_bytes()`. The `$input_type` gets converted to an
    // `$wire_type` via `TryInto`
    ($input_type:ty, $wire_type:ty) => {
        impl Serialize for $input_type {
            fn serialize(&self, buf: &mut Vec<u8>) {
                let wire: $wire_type = (*self).try_into()
                    .expect("Should never happen, input type to wire type");
                buf.extend_from_slice(&wire.to_le_bytes());
            }
        }

        impl Deserialize for $input_type {
            fn deserialize(orig_ptr: &mut &[u8]) -> Option<Self> {
                // Get the slice pointed to by `orig_ptr`
                let ptr: &[u8] = *orig_ptr;

                // Convert the slice to a fixed-size array 
                let arr: [u8; core::mem::size_of::<$wire_type>()] =
                    ptr.get(0..core::mem::size_of::<$wire_type>())?
                        .try_into().ok()?;

                // Convert the array of bytes into the `$wire_type`
                let wire_val = <$wire_type>::from_le_bytes(arr);

                // Try to convert the wire-format type into the desired type
                let converted: $input_type = wire_val.try_into().ok()?;

                // Update the pointer
                *orig_ptr = &ptr[core::mem::size_of::<$wire_type>()..];

                // Return out the deserialized `Self`!
                Some(converted)
            }
        }
    };

    // Serialize an $input_type using `to_le_bytes()` and `from_le_bytes()`
    ($input_type:ty) => {
        serialize_le!($input_type, $input_type);
    };
}

serialize_le!(u8);
serialize_le!(u16);
serialize_le!(u32);
serialize_le!(u64);
serialize_le!(u128);
serialize_le!(i8);
serialize_le!(i16);
serialize_le!(i32);
serialize_le!(i64);
serialize_le!(i128);
serialize_le!(usize, u64);
serialize_le!(isize, i64);

/// Implement serialize for `&str`
impl Serialize for &str {
    fn serialize(&self, buf: &mut Vec<u8>) {
        // Serialize the underlying bytes of the string
        Serialize::serialize(self.as_bytes(), buf);
    }
}

/// Implement serialize for `[T]`
impl<T: Serialize> Serialize for [T] {
    fn serialize(&self, buf: &mut Vec<u8>) {
        // Serialize the number of elements
        Serialize::serialize(&self.len(), buf);

        // Serialize all of the values
        self.iter().for_each(|x| Serialize::serialize(x, buf));
    }
}

/// Implement `Serialize` for `String`
impl Serialize for String {
    fn serialize(&self, buf: &mut Vec<u8>) {
        // Serialize the underlying bytes of the string
        Serialize::serialize(self.as_bytes(), buf);
    }
}

/// Implement `Deserialize` for `String`
impl Deserialize for String {
    fn deserialize(orig_ptr: &mut &[u8]) -> Option<Self> {
        // Deserialize a vector of bytes
        let vec = <Vec<u8> as Deserialize>::deserialize(orig_ptr)?;

        // Convert it to a string and return it out
        String::from_utf8(vec).ok()
    }
}

/// Implement `Serialize` for `Vec<T>`
impl<T: Serialize> Serialize for Vec<T> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        // Serialize the number of elements
        Serialize::serialize(&self.len(), buf);

        // Serialize all of the values
        self.iter().for_each(|x| Serialize::serialize(x, buf));
    }
}

/// Implement `Deserialize` for `Vec`s that contain all `Deserialize` types
impl<T: Deserialize> Deserialize for Vec<T> {
    fn deserialize(orig_ptr: &mut &[u8]) -> Option<Self> {
        // Get the length of the vector in elements
        let len = <usize as Deserialize>::deserialize(orig_ptr)?;

        // Allocate the vector we're going to return
        let mut vec = Vec::with_capacity(len);

        // Deserialize all the components
        for _ in 0..len {
            vec.push(<T as Deserialize>::deserialize(orig_ptr)?);
        }

        // Return out the deserialized vector
        Some(vec)
    }
}

/// Implement `Serialize` trait for arrays of types which implement `Serialize`
macro_rules! serialize_arr {
    ($arrsize:expr, $($foo:expr),*) => {
        impl<T: Serialize> Serialize for [T; $arrsize] {
            fn serialize(&self, buf: &mut Vec<u8>) {
                // Serialize all of the values
                self.iter().for_each(|x| Serialize::serialize(x, buf));
            }
        }

        impl<T: Deserialize> Deserialize for [T; $arrsize] {
            fn deserialize(_orig_ptr: &mut &[u8]) -> Option<Self> {
                Some([$(
                    {let _ = $foo; Deserialize::deserialize(_orig_ptr)?},
                )*])
            }
        }
    }
}

serialize_arr!( 0,);
serialize_arr!( 1, 0);
serialize_arr!( 2, 0, 0);
serialize_arr!( 3, 0, 0, 0);
serialize_arr!( 4, 0, 0, 0, 0);
serialize_arr!( 5, 0, 0, 0, 0, 0);
serialize_arr!( 6, 0, 0, 0, 0, 0, 0);
serialize_arr!( 7, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!( 8, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!( 9, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(23, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(24, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
serialize_arr!(32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

macro_rules! ser {
    // Create a new structure with serialize implemented
    (serialize, struct $structname:ident { $($id:ident: $typ:ty),*$(,)?}) => {
        ser!(defstruct, $structname, $($id, $typ)*);
        ser!(impl_serialize, $structname, $($id)*);
    };

    // Create a new structure with deserialize implemented
    (deserialize, struct $structname:ident { $($id:ident: $typ:ty),*$(,)?} ) => {
        ser!(defstruct, $structname, $($id, $typ)*);
        ser!(impl_deserialize, $structname, $($id)*);
    };

    // Create a new structure with serialize and deserialize implemented
    (serialize, deserialize, struct $structname:ident { $($id:ident: $typ:ty),*$(,)?} ) => {
        ser!(defstruct, $structname, $($id, $typ)*);
        ser!(impl_serialize, $structname, $($id)*);
        ser!(impl_deserialize, $structname, $($id)*);
    };

    // Create a new structure with serialize and deserialize implemented
    (deserialize, serialize, struct $structname:ident { $($id:ident: $typ:ty),*$(,)?} ) => {
        ser!(defstruct, $structname, $($id, $typ)*);
        ser!(impl_serialize, $structname, $($id)*);
        ser!(impl_deserialize, $structname, $($id)*);
    };

    (defstruct, $structname:ident, $($id:ident, $typ:ty)*) => {
        #[derive(Debug)]
        struct $structname {
            $(
                $id: $typ,
            )*
        }
    };

    (impl_serialize, $structname:ident, $($id:ident)*) => {
        impl Serialize for $structname {
            fn serialize(&self, buf: &mut Vec<u8>) {
                $(
                    Serialize::serialize(&self.$id, buf);
                )*
            }
        }
    };

    (impl_deserialize, $structname:ident, $($id:ident)*) => {
        impl Deserialize for $structname {
            fn deserialize(orig_ptr: &mut &[u8]) -> Option<Self> {
                // Get the original pointer
                let mut ptr = *orig_ptr;

                // Construct this structure field-by-field
                let ret = $structname {
                    $($id: Deserialize::deserialize(&mut ptr)?),*
                };

                // Update the original pointer
                *orig_ptr = ptr;

                // Return out the result
                Some(ret)
            }
        }
    };
}

ser!(serialize, deserialize, struct Baz {
    foo: u32,
});

ser!(serialize, deserialize, struct Foo {
    foo: u32,
    baz: Baz,
    bar: u8,
});

#[test]
fn test() {
    let nested_struct = Foo {
        foo: 0xdeadbeef,
        baz: Baz { foo: 0x12345678 },
        bar: 0x32,
    };

    let mut buf = Vec::new();
    nested_struct.serialize(&mut buf);
    print!("Serialized {:#x?}\n", buf);

    let mut ptr = &buf[..buf.len()];
    print!("Deserialized {:#x?}\n", Foo::deserialize(&mut ptr));
}

