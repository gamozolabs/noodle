use core::convert::TryInto;

trait Serialize {
    fn serialize(&self, buf: &mut Vec<u8>);
}

trait Deserialize: Sized {
    fn deserialize(ptr: &mut &[u8]) -> Option<Self>;
}

/// Implement `Serialize` trait for types which provide `to_le_bytes()`
macro_rules! serialize_le {
    ($typ:ty) => {
        impl Serialize for $typ {
            fn serialize(&self, buf: &mut Vec<u8>) {
                buf.extend_from_slice(&self.to_le_bytes());
            }
        }

        impl Deserialize for $typ {
            fn deserialize(orig_ptr: &mut &[u8]) -> Option<Self> {
                // Get access to the original slice
                let ptr: &[u8] = *orig_ptr;

                // Convert the slice to `Self`
                let ret = Self::from_le_bytes(
                    ptr.get(0..std::mem::size_of::<Self>())?
                        .try_into().unwrap());

                // Update the pointer
                *orig_ptr = &ptr[std::mem::size_of::<Self>()..];

                // Return out the deserialized `Self`!
                Some(ret)
            }
        }
    }
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
//serialize_le!(usize, u64);
//serialize_le!(isize, i64);

/*
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

/// Implement serialize for Vec<T>
impl<T: Serialize> Serialize for Vec<T> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        // Serialize the number of elements
        Serialize::serialize(&self.len(), buf);

        // Serialize all of the values
        self.iter().for_each(|x| Serialize::serialize(x, buf));
    }
}

/// Implement `Serialize` trait for arrays of types which implement `Serialize`
macro_rules! serialize_arr {
    ($arrsize:expr) => {
        impl<T: Serialize> Serialize for [T; $arrsize] {
            fn serialize(&self, buf: &mut Vec<u8>) {
                // Serialize all of the values
                self.iter().for_each(|x| Serialize::serialize(x, buf));
            }
        }
    }
}

serialize_arr!(0);
serialize_arr!(1);
serialize_arr!(2);
serialize_arr!(3);
serialize_arr!(4);
serialize_arr!(5);
serialize_arr!(6);
serialize_arr!(7);
serialize_arr!(8);
serialize_arr!(9);
serialize_arr!(10);
serialize_arr!(11);
serialize_arr!(12);
serialize_arr!(13);
serialize_arr!(14);
serialize_arr!(15);
serialize_arr!(16);
serialize_arr!(17);
serialize_arr!(18);
serialize_arr!(19);
serialize_arr!(20);
serialize_arr!(21);
serialize_arr!(22);
serialize_arr!(23);
serialize_arr!(24);
serialize_arr!(25);
serialize_arr!(26);
serialize_arr!(27);
serialize_arr!(28);
serialize_arr!(29);
serialize_arr!(30);
serialize_arr!(31);
serialize_arr!(32);*/

macro_rules! ser {
    (
        struct $structname:ident {
            $($id:ident: $typ:ty),*$(,)?
        }
    ) => {
        #[derive(Debug)]
        struct $structname {
            $(
                $id: $typ,
            )*
        }
        
        impl Serialize for $structname {
            fn serialize(&self, buf: &mut Vec<u8>) {
                $(
                    Serialize::serialize(&self.$id, buf);
                )*
            }
        }

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
    }
}

ser!(struct Baz {
    foo: u32,
});

impl Drop for Baz {
    fn drop(&mut self) {
        print!("DROPPIN STUFFS\n");
    }
}

ser!(struct Foo {
    foo: u32,
    bar: u8,
    baz: Baz,
    bat: u16,
});

#[test]
fn test() {
    let nested_struct = Foo {
        foo: 0xdeadbeef,
        bar: 0x32,
        baz: Baz { foo: 0x12345678 },
        bat: 0x9999,
    };

    let mut buf = Vec::new();
    nested_struct.serialize(&mut buf);
    print!("Serialized {:#x?}\n", buf);

    let mut ptr = &buf[..buf.len()-0];
    print!("Deserialized {:#x?}\n", Foo::deserialize(&mut ptr));
}

