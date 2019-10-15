extern crate core;

use core::convert::TryInto;

/// Serialize a `self` into an existing vector
trait Serialize {
    fn serialize(&self, buf: &mut Vec<u8>);
}

/// Deserialize a buffer, creating a Some(`Self`) if the serialization succeeds,
/// otherwise a `None` is returned. `ptr` should be a mutable reference to a
/// slice, this allows the deserializer to "consume" bytes by advancing the
/// pointer. To see how many bytes were deserialized, you can check the
/// difference in the `ptr`'s length before and after the call to deserialize.
///
/// If deserialization fails at any point, all intermediate objects created
/// will be destroyed, and the `ptr` will not be be changed.
///
/// This effectively behaves the same as `std::io::Read`. Since we don't have
/// `std` access in this lib we opt to go this route.
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

// Implement serialization for all of the primitive types
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
        // Make a copy of the original pointer
        let mut ptr = *orig_ptr;

        // Deserialize a vector of bytes
        let vec = <Vec<u8> as Deserialize>::deserialize(&mut ptr)?;

        // Convert it to a string and return it out
        let ret = String::from_utf8(vec).ok()?;

        // Update the original pointer
        *orig_ptr = ptr;
        Some(ret)
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
        // Make a copy of the original pointer
        let mut ptr = *orig_ptr;

        // Get the length of the vector in elements
        let len = <usize as Deserialize>::deserialize(&mut ptr)?;

        // Allocate the vector we're going to return
        let mut vec = Vec::with_capacity(len);

        // Deserialize all the components
        for _ in 0..len {
            vec.push(<T as Deserialize>::deserialize(&mut ptr)?);
        }

        // Update original pointer and return out the deserialized vector
        *orig_ptr = ptr;
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
            fn deserialize(orig_ptr: &mut &[u8]) -> Option<Self> {
                // Make a copy of the original pointer
                let mut _ptr = *orig_ptr;

                // Deserialize the array
                let arr = [$(
                    {let _ = $foo; Deserialize::deserialize(&mut _ptr)?},
                )*];

                // Update the original pointer and return out the array
                *orig_ptr = _ptr;
                Some(arr)
            }
        }
    }
}

// Implement serialization and deserialization for all arrays of types which
// are serializable and/or deserialiable up to fixed-width 32 entry arrays
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

#[macro_export]
macro_rules! ser {
    // Create a new structure with serialize implemented
    (serialize, $(#[$attr:meta])* struct $structname:ident { $($id:ident: $typ:ty),*$(,)?}) => {
        ser!(defstruct, $($attr)*, $structname, $($id, $typ)*);
        ser!(impl_serialize, $structname, $($id)*);
    };

    // Create a new structure with deserialize implemented
    (deserialize, $(#[$attr:meta])* struct $structname:ident { $($id:ident: $typ:ty),*$(,)?} ) => {
        ser!(defstruct, $($attr)*, $structname, $($id, $typ)*);
        ser!(impl_deserialize, $structname, $($id)*);
    };

    // Create a new structure with serialize and deserialize implemented
    (serialize, deserialize, $(#[$attr:meta])* struct $structname:ident { $($id:ident: $typ:ty),*$(,)?} ) => {
        ser!(defstruct, $($attr)*, $structname, $($id, $typ)*);
        ser!(impl_serialize, $structname, $($id)*);
        ser!(impl_deserialize, $structname, $($id)*);
    };

    // Create a new structure with serialize and deserialize implemented
    (deserialize, serialize, $(#[$attr:meta])* struct $structname:ident { $($id:ident: $typ:ty),*$(,)?} ) => {
        ser!(defstruct, $($attr)*, $structname, $($id, $typ)*);
        ser!(impl_serialize, $structname, $($id)*);
        ser!(impl_deserialize, $structname, $($id)*);
    };

    (defstruct, $($meta:meta)*, $structname:ident, $($id:ident, $typ:ty)*) => {
        $(
            #[$meta]
        )*
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

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn benchmark() {
        ser!(serialize, deserialize,
            #[derive(Debug, Default)]
            struct Baz {
                a: u32,
                b: u128,
                c: usize,
                d: u8,
                e: u128,
                f: i16,
            }
        );

        ser!(serialize, deserialize,
            #[derive(Debug, Default)]
            struct Foo {
                bar: [u128; 32],
            }
        );

        let nested_struct = Foo::default();

        const NUM_ITERS:  usize = 10_000_0000;
        const CLOCK_RATE: usize = 3_200_000_000;

        fn rdtsc() -> u64 {
            unsafe { core::arch::x86_64::_rdtsc() }
        }

        let mut buf = Vec::with_capacity(1024 * 1024);

        let start = rdtsc();
        for _ in 0..NUM_ITERS {
            buf.clear();
            nested_struct.serialize(&mut buf);
        }
        print!("Serialized size is {}\n", buf.len());
        let size_in_mb = (buf.len() as f64 / 1024. / 1024.) * NUM_ITERS as f64;
        let cycles  = rdtsc() - start;
        let elapsed = (cycles as f64) / CLOCK_RATE as f64;
        print!("Ser   {:10.4} cycles/iter\n", cycles as f64 / NUM_ITERS as f64);
        print!("Ser   {:10.4} MiB/sec\n", size_in_mb / elapsed);

        let start = rdtsc();
        for _ in 0..NUM_ITERS {
            let mut ptr = &buf[..buf.len()];
            let _ = unsafe {
                core::ptr::read_volatile(&Foo::deserialize(&mut ptr).unwrap())
            };
            assert!(ptr.len() == 0);
        }
        let size_in_mb = (buf.len() as f64 / 1024. / 1024.) * NUM_ITERS as f64;
        let cycles  = rdtsc() - start;
        let elapsed = (cycles as f64) / CLOCK_RATE as f64;
        print!("Deser {:10.4} cycles/iter\n", cycles as f64 / NUM_ITERS as f64);
        print!("Deser {:10.4} MiB/sec\n", size_in_mb / elapsed);
    }
}

