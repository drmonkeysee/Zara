pub const LIB_INFO: LibInfo = LibInfo {
    name: env!("CARGO_PKG_NAME"),
    version: Version {
        full: env!("CARGO_PKG_VERSION"),
        major: env!("CARGO_PKG_VERSION_MAJOR"),
        minor: env!("CARGO_PKG_VERSION_MINOR"),
        patch: env!("CARGO_PKG_VERSION_PATCH"),
    },
};

pub struct LibInfo<'a> {
    pub name: &'a str,
    pub version: Version<'a>,
}

pub struct Version<'a> {
    pub full: &'a str,
    pub major: &'a str,
    pub minor: &'a str,
    pub patch: &'a str,
}
