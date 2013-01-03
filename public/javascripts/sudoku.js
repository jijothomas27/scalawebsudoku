function SudokuController ($scope,$http)  {

	$scope.message = "hello";
	$scope.selectedClass = "active";
	
	$scope.level = 1;
	$scope.puzzle = null;
	$scope.answer = null;
	
	$scope.getSelectedClass = function (lvl) {
		if(lvl == $scope.level)
			return "active";
		else
			return '';
	};
	
	$scope.setLevel = function (lvl) {
		$scope.level = lvl;
		$scope.getPuzzle();
	};
	
	$scope.getPuzzle = function () {
		$http.get('/sudoku/'+$scope.level).success(function (data) {
			$scope.puzzle = data;
		});
	};
	
	$scope.isEditable = function (cell) {
	    if (cell == '' || cell == ' ')
	    	return true
	    else
	    	return false;
	}
	
	$scope.getCellContent = function (cell) {
	   var numPat = /[0-9]/;
	   
	   if (numPat.test(cell)) {
	   	return cell;
	   }
	   else {
	   	return '';
	   }
	}
	
	$scope.onChange = function (item) {
		alert("something changed");
	}
	
	$scope.checkSomething = function () {
		var grid = [];
		for(var i=0;i<9;i++) {
			var row = [];
			for (j=0;j<9;j++) {
			    var idx = i*9+j;
			    var val = $("td:eq("+idx+")").text();
			    //alert(val);
			    row.push(val);
			}
			grid.push(row);
		}
		
		$scope.answer = grid;
	}
	$scope.getPuzzle();
}