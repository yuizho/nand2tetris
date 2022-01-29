use std::io::{self, Write};

pub struct Element {
    name: String,
    content: Content,
}

pub enum Content {
    Elements(Vec<Element>),
    Text(String),
}

impl Element {
    pub fn new<S: Into<String>>(name: S, content: Content) -> Self {
        Element {
            name: name.into(),
            content,
        }
    }

    pub fn write<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        use Content::*;
        match &self.content {
            Elements(elements) => {
                writeln!(writer, "<{}>", self.name)?;
                for elm in elements {
                    elm.write(writer)?;
                }
                writeln!(writer, "</{}>", self.name)?;
            }
            Text(text) => {
                writeln!(writer, "<{}> {} </{0}>", self.name, text)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::xml::{Content, Element};
    use std::io::Cursor;

    #[test]
    fn write() {
        let mut elm = Element::new(
            "expression",
            Content::Elements(vec![Element::new(
                "term",
                Content::Elements(vec![Element::new(
                    "keyword",
                    Content::Text("true".to_string()),
                )]),
            )]),
        );

        let mut cursor = Cursor::new(Vec::new());
        elm.write(&mut cursor);

        let actual = cursor
            .into_inner()
            .iter()
            .map(|&s| s as char)
            .collect::<String>();
        assert_eq!(
            actual,
            "<expression>
<term>
<keyword> true </keyword>
</term>
</expression>
"
        );
    }
}
