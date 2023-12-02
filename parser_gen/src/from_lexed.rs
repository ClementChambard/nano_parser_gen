pub trait FromLexed {
    fn from_lexed(s: &str) -> Self;
}

impl FromLexed for String {
    fn from_lexed(s: &str) -> Self {
        s.to_string()
    }
}

fn from_lexed_int<T: Sized + TryFrom<i128>>(s: &str) -> T
where
    <T as TryFrom<i128>>::Error: std::fmt::Debug,
{
    if s.starts_with("0x") {
        let s = &s[2..];
        let mut n = 0;
        for c in s.chars() {
            n *= 16;
            match c {
                '0' => {}
                '1' => n += 1,
                '2' => n += 2,
                '3' => n += 3,
                '4' => n += 4,
                '5' => n += 5,
                '6' => n += 6,
                '7' => n += 7,
                '8' => n += 8,
                '9' => n += 9,
                'a' | 'A' => n += 10,
                'b' | 'B' => n += 11,
                'c' | 'C' => n += 12,
                'd' | 'D' => n += 13,
                'e' | 'E' => n += 14,
                'f' | 'F' => n += 15,
                _ => panic!(),
            }
        }
        n
    } else if s.starts_with("0b") {
        let s = &s[2..];
        let mut n = 0;
        for c in s.chars() {
            n *= 2;
            match c {
                '0' => {}
                '1' => n += 1,
                _ => panic!(),
            }
        }
        n
    } else if s.starts_with('0') {
        let mut n = 0;
        for c in s.chars() {
            n *= 8;
            match c {
                '0' => {}
                '1' => n += 1,
                '2' => n += 2,
                '3' => n += 3,
                '4' => n += 4,
                '5' => n += 5,
                '6' => n += 6,
                '7' => n += 7,
                _ => panic!(),
            }
        }
        n
    } else if s.starts_with('\'') {
        unimplemented!("Characters litteral are not implemented");
    } else {
        let mut n = 0;
        for c in s.chars() {
            n *= 10;
            match c {
                '0' => {}
                '1' => n += 1,
                '2' => n += 2,
                '3' => n += 3,
                '4' => n += 4,
                '5' => n += 5,
                '6' => n += 6,
                '7' => n += 7,
                '8' => n += 8,
                '9' => n += 9,
                _ => panic!(),
            }
        }
        n
    }
    .try_into()
    .unwrap()
}

impl FromLexed for i8 {
    fn from_lexed(s: &str) -> Self {
        from_lexed_int(s)
    }
}

impl FromLexed for i16 {
    fn from_lexed(s: &str) -> Self {
        from_lexed_int(s)
    }
}

impl FromLexed for i32 {
    fn from_lexed(s: &str) -> Self {
        from_lexed_int(s)
    }
}

impl FromLexed for i64 {
    fn from_lexed(s: &str) -> Self {
        from_lexed_int(s)
    }
}

impl FromLexed for u8 {
    fn from_lexed(s: &str) -> Self {
        from_lexed_int(s)
    }
}

impl FromLexed for u16 {
    fn from_lexed(s: &str) -> Self {
        from_lexed_int(s)
    }
}

impl FromLexed for u32 {
    fn from_lexed(s: &str) -> Self {
        from_lexed_int(s)
    }
}

impl FromLexed for u64 {
    fn from_lexed(s: &str) -> Self {
        from_lexed_int(s)
    }
}

impl FromLexed for f32 {
    fn from_lexed(s: &str) -> Self {
        let mut s = s;
        if s.ends_with('f') {
            s = s.strip_suffix('f').unwrap();
        }
        s.parse().unwrap()
    }
}

impl FromLexed for f64 {
    fn from_lexed(s: &str) -> Self {
        let mut s = s;
        if s.ends_with('f') {
            s = s.strip_suffix('f').unwrap();
        }
        s.parse().unwrap()
    }
}
