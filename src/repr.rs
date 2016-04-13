

#[derive(Debug)]
pub enum AstNode {
    // TODO
    Const(i64),

    // symbol table id
    Var(u32),

    // callee, args
    Inv(Box<AstNode>, Vec<AstNode>),
}

impl AstNode {
    pub fn int_const(val: i64) -> Self {
        AstNode::Const(val)
    }

    pub fn var(id: u32) -> Self {
        AstNode::Var(id)
    }

    pub fn inv(callee: AstNode, args: Vec<AstNode>) -> Self {
        AstNode::Inv(Box::new(callee), args)
    }
}
