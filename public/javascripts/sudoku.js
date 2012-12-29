function SudokuController ($scope,$http)  {

	$scope.message = "hello";
	$scope.selectedClass = "active";
	
	$scope.level = 1;
	$scope.puzzle = null;
	
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
	$scope.getPuzzle();
}