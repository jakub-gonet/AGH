% We used direct mapping from names to measurement data here and additional map used to translate coordinates to names
% This allows matching duplicate name or coords easily
%
% Measurements data has following shape: #{{Date, Type} => Value}
% which prevents adding measurement with same date and type within given station
-record(monitor, {coordToName = #{}, data = #{}}).
