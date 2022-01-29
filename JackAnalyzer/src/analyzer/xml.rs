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
    pub fn new_element<S: Into<String>>(name: S, child: Element) -> Self {
        Element {
            name: name.into(),
            content: Content::Elements(vec![child]),
        }
    }

    pub fn new_elements<S: Into<String>>(name: S, children: Vec<Element>) -> Self {
        Element {
            name: name.into(),
            content: Content::Elements(children),
        }
    }

    pub fn new_text<S: Into<String>>(name: S, text: S) -> Self {
        Element {
            name: name.into(),
            content: Content::Text(text.into()),
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
        let elm = Element::new_element(
            "expression",
            Element::new_element("term", Element::new_text("keyword", "true")),
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
