

/*

parse pub-defs, priv-defs, and non-defs
create initial TypeEnv, DefEnv
pub-defs, TypeEnv, DefEnv => TypedAsts, TypeEnv2
type infer priv-defs using TypeEnv2 => TypedAsts, TypeEnv3
add typed priv-def-asts to DefEnv -> DefEnv2
type inder non-defs using TypeEnv3 => TypedAsts

interpret typed-non-def-asts using DefEnv2

*/


// TODO: try_match(&TypeAcceptor, &[TypeId]) -> Result<TypeMatch, PartialMatch>


#[derive(Copy, Clone, PartialEq, Eq)]
pub struct TypeId(u64);

pub enum Def {
    Public {
        type_sig: TypeAcceptor,
        doc_string: Option<String>,
    }
    Private {
        type_sig: Option<TypeAcceptor>,
    }
}

// regular language of types (over alphabet of TypeIds)
pub enum TypeAcceptor {
    Void, // the "empty string"
    Type(TypeId),
    Union(Vec<TypeAcceptor>),
    Concat(Vec<TypeAcceptor>),
    Star(Box<TypeAcceptor>),
}

// upgrade to actually compute match
impl TypeAcceptor {
    pub fn accepts(&self, t: &[TypeId]) -> bool {
        match *self {
            Void => t.is_empty(),
            Type(id) => t.len() == 1 && t[0] == id,
            Union(ref a) => t.len() == 1 && any(combinations(a, t).map(|(a, t)| a.accepts(&[t]))),
            Concat(ref a) => t.len() == a.len() && all(zip(a, t).map(|(a, t)| a.accepts(&[t]))),
            Star(ref a) => true, // right?
        }
    }
}

pub enum Type {
    Data {
        id: TypeId
    },
    Fn {
        id: TypeId,
        args: Vec<Type>,
        ret:  Box<Type>,
    },
}
