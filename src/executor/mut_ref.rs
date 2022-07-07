use std::ops::{Deref, DerefMut};

pub enum MutRef<'a, T> {
    Borrow(&'a mut T),
    Owned(T),
}

impl<'a, T> MutRef<'a, T> {
    pub fn from_ref(mut_ref: &'a mut T) -> Self {
        Self::Borrow(mut_ref)
    }

    pub fn from_value(value: T) -> Self {
        Self::Owned(value)
    }
}

impl<'a, T> Deref for MutRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        match self {
            MutRef::Borrow(reference) => reference,
            MutRef::Owned(value) => value,
        }
    }
}

impl<'a, T> DerefMut for MutRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            MutRef::Borrow(reference) => reference,
            MutRef::Owned(value) => value,
        }
    }
}
