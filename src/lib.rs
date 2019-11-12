extern crate core;

use core::convert::TryInto;
use std::borrow::Cow;

/// Serialize a `self` into an existing vector
pub trait Serialize {
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
pub trait Deserialize: Sized {
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
impl Serialize for str {
    fn serialize(&self, buf: &mut Vec<u8>) {
        // Serialize the underlying bytes of the string
        Serialize::serialize(self.as_bytes(), buf);
    }
}

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

/// Implement `Serialize` for `Option`
impl<T: Serialize> Serialize for Option<T> {
    fn serialize(&self, buf: &mut Vec<u8>) {
        if let Some(val) = self.as_ref() {
            // Serialize that this is a some type
            buf.push(1);

            // Serialize the underlying bytes of the value
            Serialize::serialize(val, buf);
        } else {
            // `None` value case
            buf.push(0);
        }
    }
}

/// Implement `Deserialize` for `Option`
impl<T: Deserialize> Deserialize for Option<T> {
    fn deserialize(orig_ptr: &mut &[u8]) -> Option<Self> {
        // Make a copy of the original pointer
        let mut ptr = *orig_ptr;

        // Get if this option is a `Some` value
        let is_some = <u8 as Deserialize>::deserialize(&mut ptr)? != 0;
        
        let ret = if is_some {
            // Deserialize payload
            Some(<T as Deserialize>::deserialize(&mut ptr)?)
        } else {
            None
        };

        // Update the original pointer
        *orig_ptr = ptr;
        Some(ret)
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

impl<'a, T: 'a> Serialize for Cow<'a, T>
        where T: Serialize + ToOwned + ?Sized {
    fn serialize(&self, buf: &mut Vec<u8>) {
        Serialize::serialize(self.as_ref(), buf);
    }
}

impl<'a, T: 'a> Deserialize for Cow<'a, T>
        where T: ToOwned + ?Sized,
              <T as ToOwned>::Owned: Deserialize,
              Cow<'a, T>: From<<T as ToOwned>::Owned> {
    fn deserialize(orig_ptr: &mut &[u8]) -> Option<Self> {
        // Make a copy of the original pointer
        let mut ptr = *orig_ptr;

        // Deserialize into the owned type for the `Cow`
        let ret =
            <<T as ToOwned>::Owned as Deserialize>::deserialize(&mut ptr)?;

        // Update the original pointer
        *orig_ptr = ptr;
        Some(Cow::from(ret))
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

macro_rules! tuple_match {
    ($self:ident, $count:expr, $buf:expr, $enumname:ident, $enumident:ident) => {
        if let $enumname::$enumident() = $self {
            // Serialize the variant ID
            Serialize::serialize($count, $buf);
        }
    };

    ($self:ident, $count:expr, $buf:expr, $enumname:ident, $enumident:ident, $ty:ty) => {
        if let $enumname::$enumident( a ) = $self {
            // Serialize the variant ID
            Serialize::serialize($count, $buf);

            // Serialize out the fields
            Serialize::serialize(a, $buf);
        }
    };

    ($self:ident, $count:expr, $buf:expr, $enumname:ident, $enumident:ident, $_0:ty, $_1:ty) => {
        if let $enumname::$enumident( a, b ) = $self {
            // Serialize the variant ID
            Serialize::serialize($count, $buf);

            // Serialize out the fields
            Serialize::serialize(a, $buf);
            Serialize::serialize(b, $buf);
        }
    };
}

#[macro_export]
macro_rules! noodle {
    // Create a new structure with serialize implemented
    (serialize, $(#[$attr:meta])* $vis:vis struct $structname:ident { $($(#[$fattr:meta])* $fvis:vis $id:ident: $typ:ty),*$(,)?}) => {
        noodle!(defstruct, $($attr)*, $vis, $structname, $($($fattr)*, $fvis, $id, $typ)*);
        noodle!(impl_serialize, $structname, $($id)*);
    };

    // Create a new structure with deserialize implemented
    (deserialize, $(#[$attr:meta])* $vis:vis struct $structname:ident { $($(#[$fattr:meta])* $fvis:vis $id:ident: $typ:ty),*$(,)?} ) => {
        noodle!(defstruct, $($attr)*, $vis, $structname, $($($fattr)*, $fvis, $id, $typ)*);
        noodle!(impl_deserialize, $structname, $($id)*);
    };

    // Create a new structure with serialize and deserialize implemented
    (serialize, deserialize, $(#[$attr:meta])* $vis:vis struct $structname:ident { $($(#[$fattr:meta])* $fvis:vis $id:ident: $typ:ty),*$(,)?} ) => {
        noodle!(defstruct, $($attr)*, $vis, $structname, $($($fattr)*, $fvis, $id, $typ)*);
        noodle!(impl_serialize, $structname, $($id)*);
        noodle!(impl_deserialize, $structname, $($id)*);
    };

    // Create a new structure with serialize and deserialize implemented
    (deserialize, serialize, $(#[$attr:meta])* $vis:vis struct $structname:ident { $($(#[$fattr:meta])* $fvis:vis $id:ident: $typ:ty),*$(,)?} ) => {
        noodle!(defstruct, $($attr)*, $vis, $structname, $($($fattr)*, $fvis, $id, $typ)*);
        noodle!(impl_serialize, $structname, $($id)*);
        noodle!(impl_deserialize, $structname, $($id)*);
    };

    (defstruct, $($meta:meta)*, $vis:vis, $structname:ident, $($($fattr:meta)*, $fvis:vis, $id:ident, $typ:ty)*) => {
        $(
            #[$meta]
        )*
        $vis struct $structname {
            $(
                $(#[$fattr])* $fvis $id: $typ,
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

    // Create a new enum with serialize and deserialize implemented
    (serialize, deserialize,
        $(#[$attr:meta])* $vis:vis enum $enumname:ident {
            // Go through each variant in the enum
            $(
                // Variant attributes
                $(#[$variant_attr:meta])*

                // Identifier for the enum variant, always present
                $variant_ident:ident
                
                // An enum item struct
                $({
                    $(
                        $(#[$named_attr:meta])* $named_field:ident: $named_type:ty
                    ),*$(,)?
                })?

                // An enum item tuple
                $((
                    $(
                        $(#[$tuple_meta:meta])* $tuple_typ:ty
                    ),*$(,)? 
                ))?

                // An enum discriminant
                $(= $expr:expr)?
            ),*$(,)?
        }
    ) => {
        noodle!(defenum,
            $(#[$attr])* $vis enum $enumname {
                // Go through each variant in the enum
                $(
                    // Variant attributes
                    $(#[$variant_attr])*

                    // Identifier for the enum variant, always present
                    $variant_ident
                    
                    // An enum item struct
                    $({
                        $(
                            $(#[$named_attr])* $named_field: $named_type
                        ),*
                    })?

                    // An enum item tuple
                    $((
                        $(
                            $(#[$tuple_meta])* $tuple_typ
                        ),*
                    ))?

                    // An enum discriminant
                    $(= $expr)?
                ),*
            });
        noodle!(impl_serialize_enum,
            $(#[$attr])* $vis enum $enumname {
                // Go through each variant in the enum
                $(
                    // Variant attributes
                    $(#[$variant_attr])*

                    // Identifier for the enum variant, always present
                    $variant_ident
                    
                    // An enum item struct
                    $({
                        $(
                            $(#[$named_attr])* $named_field: $named_type
                        ),*
                    })?

                    // An enum item tuple
                    $((
                        $(
                            $(#[$tuple_meta])* $tuple_typ
                        ),*
                    ))?

                    // An enum discriminant
                    $(= $expr)?
                ),*
            });

        //noodle!(impl_serialize_enum, $enumname, $($variantid $($varfieldident)|*),*);
        //noodle!(impl_deserialize_enum, $enumname, $($variantid $($varfieldident)|*),*);
    };

    (defenum,
        $(#[$attr:meta])* $vis:vis enum $enumname:ident {
            // Go through each variant in the enum
            $(
                // Variant attributes
                $(#[$variant_attr:meta])*

                // Identifier for the enum variant, always present
                $variant_ident:ident
                
                // An enum item struct
                $({
                    $(
                        $(#[$named_attr:meta])* $named_field:ident: $named_type:ty
                    ),*$(,)?
                })?

                // An enum item tuple
                $((
                    $(
                        $(#[$tuple_meta:meta])* $tuple_typ:ty
                    ),*$(,)? 
                ))?

                // An enum discriminant
                $(= $expr:expr)?
            ),*$(,)?
        }) => {
            // Just define the enum as is
            $(#[$attr])* $vis enum $enumname {
                // Go through each variant in the enum
                $(
                    // Variant attributes
                    $(#[$variant_attr])*

                    // Identifier for the enum variant, always present
                    $variant_ident
                    
                    // An enum item struct
                    $({
                        $(
                            $(#[$named_attr])* $named_field: $named_type
                        ),*
                    })?

                    // An enum item tuple
                    $((
                        $(
                            $(#[$tuple_meta])* $tuple_typ
                        ),*
                    ))?

                    // An enum discriminant
                    $(= $expr)?
                ),*
            }
    };

    (impl_serialize_enum,
        $(#[$attr:meta])* $vis:vis enum $enumname:ident {
            // Go through each variant in the enum
            $(
                // Variant attributes
                $(#[$variant_attr:meta])*

                // Identifier for the enum variant, always present
                $variant_ident:ident
                
                // An enum item struct
                $({
                    $(
                        $(#[$named_attr:meta])* $named_field:ident: $named_type:ty
                    ),*$(,)?
                })?

                // An enum item tuple
                $((
                    $(
                        $(#[$tuple_meta:meta])* $tuple_typ:ty
                    ),*$(,)? 
                ))?

                // An enum discriminant
                $(= $expr:expr)?
            ),*$(,)?
        }) => {
        impl Serialize for $enumname {
            fn serialize(&self, buf: &mut Vec<u8>) {
                let mut _count = 0u32;

                // Go through each variant
                $(
                    // Struct
                    $(
                        if let $enumname::$variant_ident { $($named_field),* } = self {
                            // Serialize the variant ID
                            Serialize::serialize(&_count, buf);

                            // Serialize all fields
                            $(
                                Serialize::serialize($named_field, buf);
                            )*
                        }
                    )?

                    // Tuple
                    $(
                        tuple_match!(self, &_count, buf, $enumname, $variant_ident $(, $tuple_typ)*);
                    )?

                    _count += 1;
                )*
            }
        }
    };

    /*
    (impl_deserialize_enum, $enumname:ident, $($variantid:ident $($varfieldident:ident)|*),*) => {
        impl Deserialize for $enumname {
            fn deserialize(orig_ptr: &mut &[u8]) -> Option<Self> {
                // Get the original pointer
                let mut ptr = *orig_ptr;

                // Count to keep track of enum ids
                let mut _count = 0u32;

                // Enum identifer
                let enum_id = <u32 as Deserialize>::deserialize(&mut ptr)?;

                $(
                    if _count == enum_id {
                        let ret = $enumname::$variantid {
                            $(
                                $varfieldident: Deserialize::deserialize(&mut ptr)?,
                            )*
                        };

                        // Update pointer and return correct enum variant
                        *orig_ptr = ptr;
                        return Some(ret);
                    }

                    _count += 1;
                )*

                // Failed to deserialize
                None
            }
        }
    };*/
}

#[cfg(test)]
mod test {
    #![allow(unused)]

    use crate::*;

    #[test]
    fn test() {
        noodle!(serialize, deserialize,
            enum TestA {}
        );

        noodle!(serialize, deserialize,
            enum TestB {
                Apples,
                Bananas,
            }
        );

        noodle!(serialize, deserialize,
            enum TestC {
                Apples,
                Bananas
            }
        );

        noodle!(serialize, deserialize,
            enum TestD {
                #[cfg(test)]
                Apples {},
                Cars,
                Bananas {
                    #[cfg(test)]
                    x: u32,
                    z: i32
                },
                Cake(),
                Cakes(u32),
                Testing(i128, i64),
            }
        );

        let mut buf = Vec::new();
        TestD::Testing(0x1337133713371337, -230).serialize(&mut buf);
        print!("{:x?}\n", buf);

        noodle!(serialize, deserialize,
            enum TestE {
                Apples = 5,
                Bananas,
            }
        );

        panic!("NOPE");
    }
}

