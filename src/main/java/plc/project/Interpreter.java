package plc.project;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

public class Interpreter implements Ast.Visitor<Environment.PlcObject> {

    private Scope scope = new Scope(null);

    public Interpreter(Scope parent) {
        scope = new Scope(parent);
        scope.defineFunction("print", 1, args -> {
            System.out.println(args.get(0).getValue());
            return Environment.NIL;
        });
    }

    /**
     * Helper function to ensure an object is of the appropriate type.
     */
    private static <T> T requireType(Class<T> type, Environment.PlcObject object) {
        if (type.isInstance(object.getValue())) {
            return type.cast(object.getValue());
        } else {
            throw new RuntimeException("Expected type " + type.getName() + ", received " + object.getValue().getClass().getName() + ".");
        }
    }

    public Scope getScope() {
        return scope;
    }

    @Override
    public Environment.PlcObject visit(Ast.Source ast) {
        for (Ast.Field field : ast.getFields()) {
            visit(field);
        }

        for (Ast.Method method : ast.getMethods()) {
            visit(method);
        }

        try {
            Ast.Expression.Function mainFunction = new Ast.Expression.Function(Optional.empty(), "main", List.of());
            return visit(mainFunction);
        } catch (RuntimeException e) {
            throw new RuntimeException("No main function found.");
        }
    }

    @Override
    public Environment.PlcObject visit(Ast.Field ast) {
        Environment.PlcObject value;
        if (ast.getValue().isPresent()) {
            value = visit(ast.getValue().get());
        } else {
            value = Environment.NIL;
        }

        scope.defineVariable(ast.getName(), true, value);

        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Method ast) {
        Function<List<Environment.PlcObject>, Environment.PlcObject> function = (arguments) -> {
            scope = new Scope(scope);

            try {
                List<String> parameters = ast.getParameters();
                for (int i = 0; i < parameters.size(); i++) {
                    scope.defineVariable(parameters.get(i), false, arguments.get(i));
                }

                for (Ast.Statement smt : ast.getStatements()) {
                    visit(smt);
                }

                return Environment.NIL;
            } catch (Return returnValue) {
                return returnValue.value;
            } finally {
                scope = scope.getParent();
            }
        };

        scope.defineFunction(ast.getName(), ast.getParameters().size(), function);

        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.Expression ast) {
        visit(ast.getExpression());

        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.Declaration ast) {
        Environment.PlcObject value;

        if (ast.getValue().isPresent()) {
            value = visit(ast.getValue().get());
        } else {
            value = Environment.NIL;
        }

        scope.defineVariable(ast.getName(), false, value);

        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.Assignment ast) {
        if (!(ast.getReceiver() instanceof Ast.Expression.Access receiver)) {
            throw new RuntimeException("The left-hand side of an assignment must be a variable or field.");
        }

        Environment.PlcObject value = visit(ast.getValue());

        if (receiver.getReceiver().isPresent()) {
            Environment.PlcObject object = visit(receiver.getReceiver().get());

            if (!(object.getValue() instanceof Environment.PlcObject)) {
                throw new RuntimeException("The receiver of a field access must be an object.");
            }

            object.setField(receiver.getName(), value);
        } else {
            scope.lookupVariable(receiver.getName()).setValue(value);
        }

        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.If ast) {
        Environment.PlcObject conditionValue = visit(ast.getCondition());
        requireType(Boolean.class, conditionValue);

        if ((Boolean) conditionValue.getValue()) {
            try {
                scope = new Scope(scope);
                for (Ast.Statement smt : ast.getThenStatements()) {
                    visit(smt);
                }
            } finally {
                scope = scope.getParent();
            }
        } else if (ast.getElseStatements() != null) {
            try {
                scope = new Scope(scope);
                for (Ast.Statement smt : ast.getElseStatements()) {
                    visit(smt);
                }
            } finally {
                scope = scope.getParent();
            }
        }

        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.For ast) {

        visit(ast.getInitialization());

        while (true) {
            Environment.PlcObject conditionValue = visit(ast.getCondition());
            requireType(Boolean.class, conditionValue);

            if (!(Boolean) conditionValue.getValue()) {
                break;
            }

            try {
                scope = new Scope(scope);
                for (Ast.Statement smt : ast.getStatements()) {
                    visit(smt);
                }
            } finally {
                scope = scope.getParent();
            }
        }

        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.While ast) {
        while (true) {
            Environment.PlcObject conditionValue = visit(ast.getCondition());
            requireType(Boolean.class, conditionValue);

            if (!(Boolean) conditionValue.getValue()) {
                break;
            }

            try {
                scope = new Scope(scope);
                for (Ast.Statement smt : ast.getStatements()) {
                    visit(smt);
                }
            } finally {
                scope = scope.getParent();
            }
        }

        return Environment.NIL;
    }

    @Override
    public Environment.PlcObject visit(Ast.Statement.Return ast) {
        Environment.PlcObject returnValue;

        if (ast.getValue() != null) {
            returnValue = visit(ast.getValue());
        } else {
            returnValue = Environment.NIL;
        }

        throw new Return(returnValue);
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Literal ast) {
        if (ast.getLiteral() == null) {
            return Environment.NIL;
        }

        return Environment.create(ast.getLiteral());
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Group ast) {
        return visit(ast.getExpression());
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Binary ast) {
        Environment.PlcObject left = visit(ast.getLeft());
        Environment.PlcObject right = visit(ast.getRight());

        switch (ast.getOperator()) {
            case "&&":
                requireType(Boolean.class, left);
                if (!(Boolean) left.getValue()) {
                    return Environment.create(false);
                }
                requireType(Boolean.class, right);
                return Environment.create((Boolean) right.getValue());

            case "||":
                requireType(Boolean.class, left);
                if ((Boolean) left.getValue()) {
                    return Environment.create(true);
                }
                requireType(Boolean.class, right);
                return Environment.create((Boolean) right.getValue());

            case ">":
                requireType(Integer.class, left);
                requireType(Integer.class, right);
                return Environment.create((Integer) left.getValue() > (Integer) right.getValue());

            case ">=":
                requireType(Integer.class, left);
                requireType(Integer.class, right);
                return Environment.create((Integer) left.getValue() >= (Integer) right.getValue());

            case "<":
                requireType(Integer.class, left);
                requireType(Integer.class, right);
                return Environment.create((Integer) left.getValue() < (Integer) right.getValue());

            case "<=":
                requireType(Integer.class, left);
                requireType(Integer.class, right);
                return Environment.create((Integer) left.getValue() <= (Integer) right.getValue());

            case "==":
                return Environment.create(left.getValue().equals(right.getValue()));

            case "!=":
                return Environment.create(!left.getValue().equals(right.getValue()));

            case "+":
                if (left.getValue() instanceof Integer && right.getValue() instanceof Integer) {
                    return Environment.create((Integer) left.getValue() + (Integer) right.getValue());
                } else if (left.getValue() instanceof String && right.getValue() instanceof String) {
                    return Environment.create((String) left.getValue() + (String) right.getValue());
                } else {
                    throw new RuntimeException("Unsupported operation: +");
                }

            case "-":
                requireType(Integer.class, left);
                requireType(Integer.class, right);
                return Environment.create((Integer) left.getValue() - (Integer) right.getValue());

            case "*":
                requireType(Integer.class, left);
                requireType(Integer.class, right);
                return Environment.create((Integer) left.getValue() * (Integer) right.getValue());

            case "/":
                requireType(Integer.class, left);
                requireType(Integer.class, right);
                return Environment.create((Integer) left.getValue() / (Integer) right.getValue());

            default:
                throw new RuntimeException("Unsupported operator: " + ast.getOperator());
        }
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Access ast) {
        if (ast.getReceiver().isPresent()) {
            Environment.PlcObject receiver = visit(ast.getReceiver().get());

            if (receiver == null) {
                throw new RuntimeException("The receiver of a field access must be an object.");
            }

            return receiver.getField(ast.getName()).getValue();
        } else {
            return scope.lookupVariable(ast.getName()).getValue();
        }
    }

    @Override
    public Environment.PlcObject visit(Ast.Expression.Function ast) {
        List<Environment.PlcObject> arguments = new ArrayList<>();
        for (Ast.Expression argument : ast.getArguments()) {
            arguments.add(visit(argument));
        }
        
        if (ast.getReceiver().isPresent()) {
            Environment.PlcObject receiver = visit(ast.getReceiver().get());

            if (receiver == null) {
                throw new RuntimeException("The receiver of a method call must be an object.");
            }

            return receiver.callMethod(ast.getName(), arguments);
        } else {
            return scope.lookupFunction(ast.getName(), arguments.size()).invoke(arguments);
        }
    }

    /**
     * Exception class for returning values.
     */
    private static class Return extends RuntimeException {

        private final Environment.PlcObject value;

        private Return(Environment.PlcObject value) {
            this.value = value;
        }

    }

}
