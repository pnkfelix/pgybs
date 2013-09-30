mod sourcegen {
    pub mod meta {
        pub struct FileHandle<P> { path: ~str, }

        pub trait Program { }
        pub struct Certificate<P>;
        pub trait ObjectCode<P> { }

        pub trait MetaLang<P:Program, O:ObjectCode<P>> {
            fn temp_source_file(&mut self) -> FileHandle<P>;
            fn emit_source(&mut self, pgm: &P, file: FileHandle<P>) -> Certificate<P>;
            fn compile(&mut self, file: &FileHandle<P>, c: Certificate<P>) -> O;
            fn run(&mut self, _object:O)  { fail!("unimplemented"); }
        }
    }

    mod scheme {
        use super::meta::*;
        struct Lang;
        struct Prog(~str);
        impl Program for Prog { }
        impl ObjectCode<Prog> for ~str { }

        impl MetaLang<Prog,~str> for Lang {
            fn temp_source_file(&mut self) -> FileHandle<Prog> {
                FileHandle { path: ~"/tmp/tmp.sch" } // TODO should generate fresh names here.
            }
            fn emit_source(&mut self, pgm: &Prog, file: FileHandle<Prog>) -> Certificate<Prog> {
                use std::{io,path};
                let p = path::Path(file.path);
                match io::file_writer(&p, []) {
                    Err(s) => fail!(format!("emit_source {:s}", s)),
                    Ok(w) => w.write_str(**pgm),
                }

                Certificate
            }

            fn compile(&mut self, pgm: &FileHandle<Prog>, _c: Certificate<Prog>) -> ~str {
                pgm.path.clone()
            }
        }
    }

    mod java {
        use super::meta::*;
        struct Lang;
        struct Prog(~str);
        struct Class(~str);
        impl Program for Prog { }
        impl ObjectCode<Prog> for Class { }

        impl MetaLang<Prog, Class> for Lang {
            fn temp_source_file(&mut self) -> FileHandle<Prog> {
                FileHandle { path: ~"/tmp/tmp.java" } // TODO should generate fresh names here.
            }

            fn emit_source(&mut self, pgm: &Prog, file:FileHandle<Prog>) -> Certificate<Prog> {
                use std::{io,path};
                let p = path::Path(file.path);
                match io::file_writer(&p, []) {
                    Err(s) => fail!(format!("emit_source {:s}", s)),
                    Ok(w) => w.write_str(**pgm),
                }

                Certificate
            }

            fn compile(&mut self, _pgm: &FileHandle<Prog>, _c: Certificate<Prog>) -> Class {
                fail!("unimplemented");
            }

        }
    }
}
