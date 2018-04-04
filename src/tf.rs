#[derive(PartialEq, Debug)]
struct TransferFunction {
    num: PolynomialContainer,
}

#[derive(PartialEq, Debug)]
struct PolynomialContainer {
    // order: u32,
    polynomial: Vec<f32>, // string_repr: String
}

impl TransferFunction {
    pub fn new() -> TransferFunction {
        TransferFunction {
            num: PolynomialContainer {
                polynomial: Vec::new(),
            },
        }
    }
    pub fn set_poly(&mut self, v: Vec<f32>) {
        self.num.polynomial = v;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_create_tf_with_num_container() {
        let tf = TransferFunction::new();
        let num = PolynomialContainer {
            polynomial: Vec::new(),
        };
        assert_eq!(tf.num, num);
    }

    #[test]
    fn can_set_element0_in_num_to_1() {
        let mut tf = TransferFunction::new();
        let v = vec![1.0];
        tf.set_poly(v);
        // assert_eq!(tf.num, v);
    }
}
