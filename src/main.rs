use core::fmt;
use lazy_static::lazy_static;
use std::{
    collections::HashMap, error::Error, fs::File, io::Read, iter::Peekable, slice::Iter, str::Chars,
};

#[derive(Clone)]
enum Json {
    Object(Vec<(String, Json)>),
    Array(Vec<Json>),
    Bool(bool),
    Number(f64),
    String(String),
    Null,
}

#[derive(Debug, Clone)]
enum Token {
    StrLit(String),
    NumLit(String),
    Lcurly,
    Rcurly,
    Lbracket,
    Rbracket,
    Colon,
    Comma,
}

lazy_static! {
    static ref SINGLE_CHAR_TOKENS: HashMap<char, Token> = {
        HashMap::from([
            ('[', Token::Lbracket),
            (']', Token::Rbracket),
            ('{', Token::Lcurly),
            ('}', Token::Rcurly),
            (':', Token::Colon),
            (',', Token::Comma),
        ])
    };
}

#[derive(Debug)]
struct LookupError {
    msg: String,
}

impl fmt::Display for LookupError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {}", self.msg)
    }
}

impl Error for LookupError {}

impl LookupError {
    fn new(msg: String) -> Self {
        Self { msg }
    }
}

#[derive(Debug)]
struct ParsingError {
    msg: String,
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error: {}", self.msg)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.print_self())
    }
}

impl Error for ParsingError {}

impl ParsingError {
    fn new(msg: String) -> Self {
        ParsingError { msg }
    }
}

impl Token {
    fn print_self(&self) -> String {
        match self {
            Self::Lcurly => "Token::Lcurly".into(),
            Self::Rcurly => "Token::Rcurly".into(),
            Self::Lbracket => "Token::Lbracket".into(),
            Self::Rbracket => "Token::Rbracket".into(),
            Self::Colon => "Token::Colon".into(),
            Self::Comma => "Token::Comma".into(),
            Self::StrLit(s) => format!("Token::StrLit(\"{}\")", s),
            Self::NumLit(s) => format!("Token::NumLit({})", s),
        }
    }
}

impl Json {
    fn get(&self, key: &String) -> Result<Option<Json>, LookupError> {
        match self {
            Self::Object(vec) => {
                for (k, v) in vec {
                    if key != k {
                        continue;
                    }
                    return Ok(Some(v.clone()));
                }
                Ok(None)
            }
            _ => Err(LookupError::new(
                "Key lookup not supported for non-object Json.".into(),
            )),
        }
    }
    fn index(&self, index: usize) -> Result<Option<Json>, LookupError> {
        match self {
            Self::Array(vec) => Ok(vec.get(index).cloned()),
            _ => Err(LookupError::new(
                "Indexing not supported for non-array Json.".into(),
            )),
        }
    }
    fn dbg_str(&self, original_indent: usize, indent: usize) -> String {
        match self {
            Self::Array(j) => {
                let res = j
                    .iter()
                    .map(|x| {
                        format!(
                            "\n{:indent$}{}",
                            "",
                            x.dbg_str(original_indent, indent + original_indent),
                            indent = indent
                        )
                    })
                    .fold(
                        String::new(),
                        |a, b| if a.is_empty() { b } else { a + ", " + &b },
                    );
                format!(
                    "[{}{}]",
                    res,
                    if !res.is_empty() {
                        "\n".to_string() + &" ".repeat(indent - original_indent)
                    } else {
                        "".to_string()
                    },
                )
            }
            Self::Null => String::from("null"),
            Self::Bool(b) => b.to_string(),
            Self::Object(json) => {
                let res = json
                    .iter()
                    .map(|(k, v)| {
                        format!(
                            "\n{:indent$}\"{}\": {}",
                            "",
                            k.to_owned(),
                            v.dbg_str(original_indent, indent + original_indent),
                            indent = indent
                        )
                    })
                    .fold(
                        String::new(),
                        |a, b| {
                            if a.is_empty() {
                                b
                            } else {
                                a + ", " + &b
                            }
                        },
                    );
                format!(
                    "{{{}{}}}",
                    res,
                    if !res.is_empty() {
                        "\n".to_string() + &" ".repeat(indent - original_indent)
                    } else {
                        "".to_string()
                    },
                )
            }
            Self::String(s) => format!("\"{}\"", s),
            Self::Number(n) => n.to_string(),
        }
    }
}

impl fmt::Debug for Json {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.dbg_str(4, 4))
    }
}

fn read_boolean_or_null(
    curr: &char,
    it: &mut Peekable<Chars>,
) -> Result<Option<Token>, ParsingError> {
    let mut buffer = String::from(*curr);
    while it
        .peek()
        .ok_or_else(|| ParsingError::new("Failed to identify boolean".into()))
        .is_ok_and(|&x| !(x == ',' || x == ']' || x == '}'))
    {
        let next = it.next().unwrap();
        if next.is_whitespace() {
            continue;
        }
        buffer.push(next);
    }
    match buffer.as_ref() {
        "true" | "false" | "null" => Ok(Some(Token::StrLit(buffer))),
        _ => Err(ParsingError::new(
            "Boolean or null could not be fetched (Json schema invalid)".into(),
        )),
    }
}

fn read_string(it: &mut Peekable<Chars>) -> Result<Token, ParsingError> {
    let mut buffer = String::new();
    loop {
        if it.peek().is_none() {
            return Err(ParsingError::new("Failed to close string literal.".into()));
        }
        let chr = it.next().unwrap();
        if chr == '"' {
            break;
        }
        buffer.push(chr);
    }
    Ok(Token::StrLit(buffer))
}

fn read_numeric(curr: &char, it: &mut Peekable<Chars>) -> Result<Token, ParsingError> {
    let mut buffer = String::new();
    buffer.push(*curr);
    loop {
        if it.peek().is_none() {
            return Err(ParsingError::new("Failed to close numeric.".into()));
        }
        let chr = *it.peek().unwrap();
        if chr.is_digit(10) || chr == '.' {
            buffer.push(chr);
            let _ = it.next();
        } else {
            break;
        }
    }
    Ok(Token::NumLit(buffer))
}

fn tokenize(string: &String) -> Result<Vec<Token>, ParsingError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut it = string.chars().into_iter().peekable();
    while let Some(char) = it.next() {
        if let Some(token_match) = SINGLE_CHAR_TOKENS.get(&char) {
            tokens.push(token_match.clone());
        } else if char == '"' {
            tokens.push(read_string(&mut it)?)
        } else if char.is_digit(10) || char == '.' || char == '-' {
            tokens.push(read_numeric(&char, &mut it)?);
        } else if char == 't' || char == 'f' || char == 'n' {
            let bool_match = read_boolean_or_null(&char, &mut it)?;
            if let Some(bool_token) = bool_match {
                tokens.push(bool_token);
            }
        }
    }
    Ok(tokens)
}

fn parse_array(it: &mut Peekable<Iter<Token>>) -> Result<Json, ParsingError> {
    let Some(_) = it.next() else {
        return Err(ParsingError::new("Starting [ not found.".into()));
    };
    let mut array_vec: Vec<Json> = Vec::new();
    loop {
        let Some(&&ref tk) = it.peek() else {
            return Err(ParsingError::new("Matching ] not found.".into()));
        };
        let item: Result<Json, ParsingError> = match tk {
            Token::Rbracket => {
                it.next();
                break;
            }
            Token::Comma => {
                it.next();
                continue;
            }
            _ => {
                let value: Json = match tk {
                    Token::StrLit(v) => match v.as_str() {
                        "true" => Json::Bool(true),
                        "false" => Json::Bool(false),
                        "null" => Json::Null,
                        _ => Json::String(v.to_string()),
                    },
                    Token::NumLit(v) => Json::Number(v.parse().unwrap()),
                    Token::Lcurly => parse_object(it)?,
                    Token::Lbracket => parse_array(it)?,
                    _ => {
                        return Err(ParsingError::new(format!(
                            "Json value not parsable. Token: {}",
                            tk,
                        )))
                    }
                };
                if let Some(&&ref peeked) = it.peek() {
                    let _ = match peeked {
                        Token::Rcurly => None,
                        Token::Rbracket => None,
                        Token::Colon => None,
                        _ => it.next(),
                    };
                }
                Ok(value)
            }
        };
        array_vec.push(item?);
    }
    Ok(Json::Array(array_vec))
}

fn parse_object(it: &mut Peekable<Iter<Token>>) -> Result<Json, ParsingError> {
    let Some(_) = it.next() else {
        return Err(ParsingError::new("Starting { not found.".into()));
    };
    let mut json_object_vec: Vec<(String, Json)> = Vec::new();
    loop {
        let Some(&&ref tk) = it.peek() else {
            return Err(ParsingError::new("Matching } not found.".into()));
        };
        let pair: Result<(String, Json), ParsingError> = match tk {
            Token::Rcurly => {
                it.next();
                break;
            }
            Token::StrLit(key) => {
                it.next();
                let is_kv_pair: bool = match it.peek() {
                    Some(&&ref colon) => match colon {
                        Token::Colon => true,
                        _ => false,
                    },
                    None => false,
                };
                if !is_kv_pair {
                    return Err(ParsingError::new(format!(
                        "Json object delimiter not found. Token: {:?}",
                        it.peek()
                    )));
                }
                it.next();
                let value: Json = match it.peek() {
                    Some(&&ref val) => match val {
                        Token::StrLit(v) => match v.as_str() {
                            "true" => Json::Bool(true),
                            "false" => Json::Bool(false),
                            "null" => Json::Null,
                            _ => Json::String(v.to_string()),
                        },
                        Token::NumLit(v) => Json::Number(v.parse().unwrap()),
                        Token::Lcurly => parse_object(it)?,
                        Token::Lbracket => parse_array(it)?,
                        _ => {
                            return Err(ParsingError::new(format!(
                                "Json value not parsable. Key: {}",
                                key
                            )))
                        }
                    },
                    None => return Err(ParsingError::new("Json value could not be found.".into())),
                };
                if let Some(&&ref peeked) = it.peek() {
                    let _ = match peeked {
                        Token::Rcurly => None,
                        Token::Rbracket => None,
                        Token::Colon => None,
                        _ => it.next(),
                    };
                }
                Ok((key.to_string(), value))
            }
            Token::Lcurly => {
                break;
            }
            Token::Comma => {
                it.next();
                continue;
            }
            _ => Err(ParsingError::new(format!(
                "Unrecognized parsing pattern. Token: {:?}",
                tk
            ))),
        };
        json_object_vec.push(pair?);
    }
    Ok(Json::Object(json_object_vec))
}

fn parse(string: &String) -> Result<Json, ParsingError> {
    let tokens = tokenize(string)?;
    let json_object = parse_object(&mut tokens.iter().peekable())?;
    Ok(json_object)
}

fn main() {
    let mut file = File::open("example.json").expect("File should have been expected.");
    let mut json_str = String::new();
    file.read_to_string(&mut json_str)
        .expect("Cannot read contents of file to string.");
    let object = parse(&json_str);
    dbg!(&object);
    /* Example lookup:
    if let Ok(object) = parse(&json_str) {
        let res = object
            .get(&"users".into())
            .expect("Users could not be found")
            .expect("Users could not be found in object")
            .index(0)
            .expect("First index not found")
            .expect("First index could not be accessed")
            .get(&"profile".into())
            .expect("Profile 1 should've been found")
            .expect("Profile 1 should've been accessed");
        dbg!(&res);
    } */
}
