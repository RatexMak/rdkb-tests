/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.webpa;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.webpa.WebPaServerResponse;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.exceptions.TestException;
import com.automatics.webpa.WebPaConnectionHandler;
//import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;

public class BroadBandWebPaPerformanceTest extends AutomaticsTestBase {

	/**
	 * Integer to store Iterations of Performance test
	 */
	private static final Integer WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_10 = 10;

	private static final Integer WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_5 = 5;
	
    /**
     * Integer to store Iterations of Performance test
     */
    private static final Integer WEBPA_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION = 10;

	/**
	 * Calculating End to End WebPA request Processing Time and Box Processing Time
	 * for WebPA Get Operation with Single Parameter for 10 Iteration
	 * <ol>
	 * <li>PRE-CONDITION :Check whether WebPA is Up and Running</li>
	 * <li>ITERATION STEP : Verify the WebPA Get Operation is successful, also End
	 * to End & Box Processing time is calculated</li>
	 * <li>Repeat the ITERATION STEP for ten times</li>
	 * <li>STEP 1: Calculate Success rate for webPA GET request with single
	 * parameter</li>
	 * <li>STEP 2: Calculate Average end to end webPA request processing time for
	 * webPA GET request with single parameter</li>
	 * </ol>
	 * 
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WEBPA-PERF-1000")
	public void wepaPerformanceTestWithOneGetParameter(Dut device) {

		boolean status = false;
		String testId = "TC-RDKB-WEBPA-PERF-100";
		String testStep = null;
		String errorMessage = null;
		Integer successCount = 0;
		long collectiveTime = 0;
		long meanAverageTime = 0;
		Integer iteration = 0;
		long endToEndProcessingTime = 0;
		long boxProcessingTime = 0;
		long collectiveBoxProcessingTime = 0;

		try {

			LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WEBPA-PERF-1000 ####################");

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"TEST DESCRIPTION: Calculating End to End WebPA request Processing Time and Box Processing Time for WebPA Get Operation with Single Param for 10 Iteration");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION : Check whether Webpa is Up and Running");
			LOGGER.info(
					"ITERATION STEP :  Verify the WebPA Get Operation is successful, also End to End & Box Processing time is calculated");
			LOGGER.info("Repeat the ITERATION STEP for ten times");
			LOGGER.info("1: Calculate Success rate for WEBPA GET request with single parameter");
			LOGGER.info(
					"2: Calculate Average end to end webpa request processing time for WEBPA GET request with single parameter");
			LOGGER.info("**********************************************************************************");

			LOGGER.info("##########################  STARTING PRE-CONFIGURATIONS ##########################");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("PRECONDITION : DESCRIPTION: VERIFY WHETHER WEBPA IS UP AND RUNNING");
			LOGGER.info(
					"PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
			LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);

			errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
			if (!status) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			} else {
				LOGGER.info("WEBPA PROCESS IS UP AND RUNNING, PROCEEDING FOR TEST!");
			}

			LOGGER.info("**********************************************************************************");
			LOGGER.info("#########################  COMPLETED PRE-CONFIGURATIONS #########################");

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"CALCULATING END TO END WEBPA REQUEST PROCESSING TIME AND BOX PROCESSING TIME FOR WEBPA GET OPERATION WITH SINGLE PARAM FOR 10 ITERATION");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("WEBPA PARAMETER USED IN EXECUTION: " + BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_MODELNAME);
			LOGGER.info("**********************************************************************************");

			// Checking atom sync to verify WEBPA logs in atom console for Atom based devices
			boolean isAtom = CommonMethods.isAtomSyncAvailable(device, tapEnv);

			for (int count = 0; count < WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_10; count++) {
				iteration = count + 1;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("ITERATION :" + iteration
						+ ": VERIFY THE WEBPA GET OPERATION IS SUCCESSFUL, ALSO END TO END & BOX PROCESSING TIME IS CALCULATED");
				LOGGER.info(
						"EXPECTED : WEBPA GET OPERATION SHOULD BE SUCCESSFUL AND END TO END & BOX PROCESSING TIME SHOULD BE OBTAINED");
				LOGGER.info("**********************************************************************************");

				List<Long> result = new BroadBandWebPaUtils().calculateEndToEndAndBoxProcessingTimeForWebPaOperation(
						tapEnv, device, isAtom, false, iteration);

				endToEndProcessingTime = result.get(BroadBandTestConstants.CONSTANT_0);
				boxProcessingTime = result.get(BroadBandTestConstants.CONSTANT_1);

				if (endToEndProcessingTime != 0 && boxProcessingTime != 0) {
					successCount++;
					collectiveTime += endToEndProcessingTime;
					collectiveBoxProcessingTime += boxProcessingTime;

					LOGGER.info("**********************************************************************************");
					LOGGER.info("WEBPA GET REQUEST BOX PROCESSING TIME IN MILLISECONDS FOR INTERATION " + iteration
							+ " : " + boxProcessingTime / 1000000.0);
					LOGGER.info("END TO END WEBPA GET REQUEST PROCESSING TIME IN MILLISECONDS FOR INTERATION "
							+ iteration + " : " + endToEndProcessingTime);
					LOGGER.info("**********************************************************************************\n");
				} else {
					LOGGER.error(
							"UNABLE TO CALCULATE END TO END/BOX PROCESSING TIME FOR WEBPA GET EXECUTION WITH SINGLE PARAM");
				}

				tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			}

			testStep = "s1";
			status = false;
			errorMessage = "SUCCESS RATE IS NOT 100%. EXPECTED SUCCESS COUNT : 10; ACTUAL SUCCESS COUNT : "
					+ successCount;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: CALCULATE SUCCESS RATE FOR WEBPA GET REQUEST WITH SINGLE PARAMETER");
			LOGGER.info("STEP 1: EXPECTED: SUCCESS RATE FOR WEBPA REQUEST WITH ONE GET PARAMETER SHOULD BE 10/10");
			LOGGER.info("**********************************************************************************");
			status = (successCount == BroadBandTestConstants.CONSTANT_10);
			LOGGER.info(
					"SUCCESS COUNT FOR WEBPA GET OPERATION WITH SINGLE PARAM OUT OF 10 ITERATION IS : " + successCount);
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL: SUCCESS RATE FOR WEBPA GET OPERATION WITH SINGLE PARAM FOR 10 ITERATION IS 100 % : "
								+ successCount + "/" + WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_10);
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

			testStep = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION: CALCULATE AVERAGE END TO END WEBPA REQUEST PROCESSING TIME FOR WEBPA GET REQUEST WITH SINGLE PARAMETER");
			LOGGER.info(
					"STEP 2: EXPECTED: AVERAGE RESPONSE TIME FOR WEBPA GET REQUEST WITH SINGLE PARAMETER SHOULD BE LESS THAN 1000 MILLISECONDS");
			LOGGER.info("**********************************************************************************");
			meanAverageTime = collectiveTime / successCount;
			LOGGER.info("AVERAGE END TO END WEBPA GET REQUEST PROCESSING TIME : " + meanAverageTime + " MILLISECONDS");
			errorMessage = "AVERAGE RESPONSE TIME FOR WEBPA REQUEST WITH ONE GET PARAMETER IS GREATER THAN 1000 MILLISECONDS. ACTUAL : "
					+ meanAverageTime + " msec";
			status = meanAverageTime <= 1000;
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: AVERAGE RESPONSE TIME FOR WEBPA GET REQUEST WITH SINGLE PARAMETER FOR "
						+ successCount + " SUCCESSFUL ITERATIONS IS :" + meanAverageTime + " MILLISECONDS");
			} else {
				LOGGER.error("STEP 2: ACTUAL:" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

			LOGGER.info("##########################################################################");
			LOGGER.info("AVERAGE END TO END WEBPA GET REQUEST PROCESSING TIME WITH SINGLE PARAM FOR " + successCount
					+ " ITERATION : " + meanAverageTime + " MILLISECONDS");
			LOGGER.info("AVERAGE BOX PROCESSING TIME FOR WEBPA GET REQUEST WITH SINGLE PARAM FOR " + successCount
					+ " ITERATION : " + ((collectiveBoxProcessingTime / successCount) / 1000000.0) + " MILLISECONDS");
			LOGGER.info("##########################################################################");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, false);
		}

		LOGGER.info("#################### ENDING TEST CASE: TC-RDK-WEBPA-PERF-1000 ####################");
	}

	/**
	 * 
	 * Calculate Average Response Time for Five Get parameters*
	 * <ol>
	 * <li>PRE-CONDITION: Check whether WebPA is Up and Running.</li>*
	 * <li>ITERATION STEP 1: Verify webPA get response from device with five
	 * parameters.</li>*
	 * <li>Repeat the ITERATION STEP 1 for five times totally 5 steps</li>*
	 * <li>STEP 1: Calculate Success rate for webPA request with Five Get
	 * parameters.</li>
	 * <li>STEP 2: Calculate Average Response Time for webPA request with Five Get
	 * parameters.</li>
	 * </ol>
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WEBPA-PERF-1001")
	public void wepaPerformanceTestWithFiveGetParamters(Dut device) {

		boolean status = false;// boolean to store the test case status
		String testId = "TC-RDKB-WEBPA-PERF-101";// Test case id
		String testStep = null;// Test step number
		String errorMessage = null;// String to store the error message
		Integer successCount = 0;// Integer to successcount
		long collectiveTime = 0;// Long to store addition of time difference
		long averageTime = 0;// Long to Average response time
		Integer iteration = 0;// Integer to store iteration value
		long timeDifference = 0;// long to store time difference

		try {
			LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WEBPA-PERF-1001 #####################");
			LOGGER.info("TEST DESCRIPTION:Calculate Average Response Time for WEBPA request with five Get parameters ");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION :Check whether Webpa is Up and Running");
			LOGGER.info("ITERATION STEP 1:Verify webpa get response from device with five parameters");
			LOGGER.info("Repeat the ITERATION STEP 1 for ten times totally 5 steps.");
			LOGGER.info("1: Calculate Success rate for WEBPA request with Five Get parameters ");
			LOGGER.info("2: Calculate Average Response Time for WEBPA request with Five Get parameters ");
			LOGGER.info("#####################################################################################");
			LOGGER.info("####################################STARTING PRE-CONFIGURATIONS############################");
			LOGGER.info("PRECONDITION:Verify whether Webpa is Up and Running");
			LOGGER.info(
					"DESCRIPTION:Verifying Successful webPA Get response, in case of failure rechecking for 8 minutes");
			LOGGER.info("##########################################################################");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			errorMessage = "webPA is not Up and Running";
			if (!status) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			LOGGER.info("###############################COMPLETED PRE-CONFIGURATIONS###############################");
			LOGGER.info("Calculation of average Response Time of webPA request with Five Get parameters");
			LOGGER.info("##########################################################################");
			LOGGER.info("ITERATION STEP 1: Verify webPA get response from device with five parameters ");
			LOGGER.info("EXPECTED:webPA get response from device with five parameters should be successfull");
			LOGGER.info("##########################################################################");
			LOGGER.info("webPA PARAMETERS USED IN FOLLOWING EXECUTION");
			String[] webpaGetParameters = BroadBandWebPaUtils.webpaGetParameters(5);
			for (int count = 0; count < webpaGetParameters.length; count++) {
				LOGGER.info(webpaGetParameters[count]);
			}
			LOGGER.info("##########################################################################");
			for (int count = 0; count < WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_5; count++) {
				iteration = count + 1;
				LOGGER.info("##########################################################################");
				LOGGER.info(
						"*****************************************************************************************");
				LOGGER.info("ITERATION STEP " + iteration + " : Verify webPA get response from device ");

				long endTime = 0;
				long startTime = 0;
				int code = 0;

				WebPaServerResponse webPaServerResponse = new WebPaServerResponse();

				// Execution of webPA get command
				startTime = System.currentTimeMillis();
				webPaServerResponse = WebPaConnectionHandler.get().getWebPaParamValue(device, webpaGetParameters);
				endTime = System.currentTimeMillis();

				code = webPaServerResponse.getStatusCode();
				if (code == 200) {
					LOGGER.info(
							"Verification of webPA get response from device is Success in iteration step " + iteration);
					timeDifference = endTime - startTime;
				} else {
					LOGGER.info(
							"Verification of webPA get response from device is Failure in iteration step " + iteration);
				}
				if (timeDifference != 0) {
					successCount++;
					collectiveTime += timeDifference;
				} else {
					LOGGER.error("Failure in capturing time difference of webPA execution");
				}
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			}

			/**
			 * STEP 1: Calculate Success rate for webPA request with Five Get parameters
			 */
			testStep = "s1";
			status = false;

			LOGGER.info("*****************************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Calculate Success rate for webPA request with Five Get parameters ");
			LOGGER.info("STEP 1: ACTION : Check Success rate for webPA request with Five Get parameters");
			LOGGER.info(
					"STEP 1: EXPECTED : Success rate for webPA request with Five Get parameters should be 5/5 rate");
			LOGGER.info("*****************************************************************************************");

			LOGGER.info("##########################################################################");
			LOGGER.info("STEP 1: DESCRIPTION: Calculate Success rate for webPA request with Five Get parameters ");
			LOGGER.info(
					"STEP 1: EXPECTED: Success rate for webPA request with Five Get parameters should be 10/10 rate");
			errorMessage = "Success rate is Zero,reason might be webPA is not responding";
			if (successCount != 0) {
				status = true;
				LOGGER.info("STEP 1: ACTUAL:Success rate for webPA request with Five Get parameters " + successCount
						+ "/" + WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_5);
			} else {
				LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

			/**
			 * STEP 2: Calculate Average Response Time for webPA request with Five Get
			 * parameters
			 */
			testStep = "s2";
			status = false;

			LOGGER.info("*****************************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Calculate Average Response Time for webPA request with Five Get parameters");
			LOGGER.info("STEP 1: ACTION : Check success rate for webPA request with five Get parameters");
			LOGGER.info("STEP 1: EXPECTED : Success rate for webPA request with Ten Get parameters should be 5/5 rate");
			LOGGER.info("*****************************************************************************************");

			averageTime = collectiveTime / successCount;
			errorMessage = "Average Response Time for webPA request with Five Get parameters is greater than 1000msec. ACTUAL RESPONSE : "
					+ averageTime + " msec";
			if (averageTime <= 1000) {
				status = true;
				LOGGER.info("STEP 2:ACTUAL:Average Response Time for webPA request with Five Get parameters for "
						+ successCount + " Successful iterations is :" + averageTime + "msec");
			} else {
				LOGGER.error("STEP 2:ACTUAL: " + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.info("Failure in executing webPA get command with five parameters \n" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
		}
 		LOGGER.info("##########################################################################");
 		LOGGER.info("Average Response Time for webPA request with Five Get parameters : " + averageTime + "msec");
 		LOGGER.info("##########################################################################");
		LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-WEBPA-PERF-1001 ####################");
	}

	/**
	 * 
	 * Calculate Average Response Time for Ten Get parameters
	 * <ol>
	 * <li>PRE-CONDITION: Check whether webPA is Up and Running.</li>*
	 * <li>ITERATION STEP 1: Verify webPA get response from device with ten
	 * parameters</li>*
	 * <li>Repeat the ITERATION STEP 1 for ten times totally 10 steps</li>*
	 * <li>STEP 1: Calculate Success rate for webPA request with Ten Get
	 * parameters.</li>
	 * <li>STEP 2: Calculate Average Response Time for webPA request with Ten Get
	 * parameters.</li>
	 * </ol>
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WEBPA-PERF-1002", testDecription = "Calculate Average Response Time for webPA request with Ten Get parameters ")
	public void wepaPerformanceTestWithTenGetParamters(Dut device) {

		boolean status = false;// String to store the test case status
		String testId = "TC-RDKB-WEBPA-PERF-102";// Test case id
		String testStep = null;// Test step number
		String errorMessage = null;// String to store the error message
		Integer successCount = 0;// Integer to successcount
		long collectiveTime = 0;// Long to store addition of time difference
		long averageTime = 0;// Long to Average response time
		Integer iteration = 0;// Integer to store iteration value
		long timeDifference = 0;// long to store time difference

		try {
			LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WEBPA-PERF-1002 #####################");
			LOGGER.info("TEST DESCRIPTION:Calculate Average Response Time for webPA request with Ten Get parameters");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION :Check whether webPA is Up and Running");
			LOGGER.info("ITERATION STEP 1:Verify webPA get response from device");
			LOGGER.info("Repeat the ITERATION STEP 1 for ten times totally 10 steps");
			LOGGER.info("1: Calculate Success rate for webPA request with Ten Get parameters ");
			LOGGER.info("2: Calculate Average Response Time for webPA request with Ten Get parameters ");
			LOGGER.info("#####################################################################################");
			LOGGER.info("####################################STARTING PRE-CONFIGURATIONS############################");
			LOGGER.info("####################################STARTING PRE-CONFIGURATIONS############################");
			LOGGER.info("PRECONDITION: DESCRIPTION: VERIFY WHETHER WEBPA IS UP AND RUNNING");
			LOGGER.info(
					"PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
			LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			errorMessage = "Webpa is not Up and Running";
			if (!status) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			LOGGER.info("###############################COMPLETED PRE-CONFIGURATIONS###############################");
			LOGGER.info("##########################################################################");
			LOGGER.info("Calculation of Average Response Time of WEBPA request with Ten Get parameters");
			LOGGER.info("*****************************************************************************************");

			LOGGER.info("*****************************************************************************************");
			LOGGER.info("ITERATION STEPS: DESCRIPTION : Verify webpa get response from device with Ten parameters");
			LOGGER.info("ITERATION STEPS: ACTION : Execute webPA get request with Ten Get parameters");
			LOGGER.info(
					"ITERATION STEPS: EXPECTED : webPA get response from device with Ten parameters should be successful");
			LOGGER.info("*****************************************************************************************");

			LOGGER.info("webPA PARAMETERS USED IN FOLLOWING EXECUTION");
			String[] webpaGetParameters = BroadBandWebPaUtils.webpaGetParameters(10);
			for (int count = 0; count < webpaGetParameters.length; count++) {
				LOGGER.info(webpaGetParameters[count]);
			}
			for (int count = 0; count < WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_10; count++) {
				iteration = count + 1;
				LOGGER.info(
						"*****************************************************************************************");
				LOGGER.info("ITERATION STEP " + iteration + " : Verify webPA get response from device ");

				long endTime = 0;
				long startTime = 0;
				int returnCode = 0;
				WebPaServerResponse webPaServerResponse = new WebPaServerResponse();
				LOGGER.info("webPaServerResponse in initial " + webPaServerResponse);
				// Execution of webPA get command
				startTime = System.currentTimeMillis();
				webPaServerResponse = WebPaConnectionHandler.get().getWebPaParamValue(device, webpaGetParameters);
				endTime = System.currentTimeMillis();

				returnCode = webPaServerResponse.getStatusCode();

				if (returnCode == 200) {
					LOGGER.info(
							"Verification of webpa get response from device is Success in iteration step " + iteration);
					timeDifference = endTime - startTime;
				} else {
					LOGGER.info(
							"Verification of webpa get response from device is Failure in iteration step " + iteration);
				}
				if (timeDifference != 0) {
					successCount++;
					collectiveTime += timeDifference;
				} else {
					LOGGER.error("Failure in capturing time difference of webpa execution");
				}
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			}

			/**
			 * STEP 1: Calculate Success rate for WEBPA request with Ten Get parameters
			 * Request
			 */
			testStep = "s1";
			status = false;
			errorMessage = "Success rate is Zero,reason might be webpa is not responding";
			LOGGER.info("*****************************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Calculate Success rate for WEBPA request with Ten Get parameters Request");
			LOGGER.info("STEP 1: ACTION : Check success rate for WEBPA request with Ten Get parameters");
			LOGGER.info(
					"STEP 1: EXPECTED : Success rate for WEBPA request with Ten Get parameters should be 10/10 rate");
			LOGGER.info("*****************************************************************************************");

			if (successCount != 0) {
				status = true;
				LOGGER.info("STEP 1:ACTUAL:Success rate for WEBPA request with Ten Get parameters is :" + successCount
						+ "/" + WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_10);
			} else {
				LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

			/**
			 * STEP 2: Calculate Average Response Time for WEBPA request with Ten Get
			 * parameters
			 */
			testStep = "s2";
			status = false;
			errorMessage = "Average Response Time for WEBPA request with Ten Get parameters is greater than 1000msec. ACTUAL RESPONSE : "
					+ averageTime + " msec";

			LOGGER.info("*****************************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Calculate Average Response Time for WEBPA request with Ten Get parameters");
			LOGGER.info(
					"STEP 2: ACTION : Check average Response Time for WEBPA request with Ten Get parameters Request is less than 1000msec");
			LOGGER.info(
					"STEP 2: EXPECTED : Average Response Time for WEBPA request with Ten Get parameters Request should be less than 1000mse");
			LOGGER.info("*****************************************************************************************");

			averageTime = collectiveTime / successCount;

			if (averageTime <= 1000) {
				status = true;
				LOGGER.info("STEP 2:ACTUAL:Average Response Time for WEBPA request with Ten Get parameters for "
						+ successCount + " Successful iterations is :" + averageTime + "msec");
			} else {
				LOGGER.error("STEP 2:ACTUAL:" + errorMessage);
			}
			LOGGER.info("##########################################################################");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Faliure in executing Webpa get command with ten parameters \n" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
		}

		LOGGER.info("##########################################################################");
		LOGGER.info("Average Response Time for Ten Get parameters: " + averageTime + "msec");
		LOGGER.info("##########################################################################");
		LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-WEBPA-PERF-1002 ####################");
	}

	/**
	 * Calculating average End to End WebPA request Processing Time and Box
	 * Processing Time for WebPA Set Operation with Single Parameter for 10 Iteration
	 * <ol>
	 * <li>PRE-CONDITION :Check whether WebPA is Up and Running</li>
	 * <li>ITERATION STEP : Verify webPa set response from device with one
	 * parameter</li>
	 * <li>Repeat the ITERATION STEP for ten times</li>
	 * <li>STEP 1: Calculate Success rate for webPA SET request with One
	 * parameter</li>
	 * <li>STEP 2: Calculate Average End To End Request Processing Time for webPA
	 * SET request with One parameter</li>
	 * <li>POST-CONDITION :Factory reset device and reactivate it</li>
	 * </ol>
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WEBPA-PERF-1003")
	public void wepaPerformanceTestOneSetParameter(Dut device) {

		boolean status = false;
		String testId = "TC-RDKB-WEBPA-PERF-103";
		String testStep = null;
		String errorMessage = null;
		Integer successCount = 0;
		long collectiveTime = 0;
		long meanAverageTime = 0;
		Integer iteration = 0;
		long endToEndProcessingTime = 0;
		long boxProcessingTime = 0;
		long collectiveBoxProcessingTime = 0;

		try {
			LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WEBPA-PERF-1003 ####################");

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"TEST DESCRIPTION: Calculating average End to End WebPA request Processing Time and Box Processing Time for WebPA Set Operation with Single Param for 10 Iteration");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION :Check whether WebPa is Up and Running");
			LOGGER.info(
					"ITERATION STEP : Verify the webpa set operation is successful, also end to end & box processing time is calculated");
			LOGGER.info("Repeat the ITERATION STEP for ten times");
			LOGGER.info("1: Calculate Success rate for WEBPA SET request with One parameterr");
			LOGGER.info(
					"2: Calculate Average End To End Request Processing Time for WEBPA SET request with One parameter");
			LOGGER.info("**********************************************************************************");

			LOGGER.info("##########################  STARTING PRE-CONFIGURATIONS ##########################");

			LOGGER.info("**********************************************************************************");
			LOGGER.info("PRECONDITION : DESCRIPTION: VERIFY WHETHER WEBPA IS UP AND RUNNING");
			LOGGER.info(
					"PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
			LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
			LOGGER.info("**********************************************************************************");

			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			errorMessage = "WEBPA PROCESS IS NOT UP AND RUNNING";
			if (!status) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			} else {
				LOGGER.info("WEBPA PROCESS IS UP AND RUNNING, PROCEEDING FOR TEST!");
			}
			LOGGER.info("**********************************************************************************");
			LOGGER.info("#########################  COMPLETED PRE-CONFIGURATIONS #########################");

			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"CALCULATING END TO END WEBPA REQUEST PROCESSING TIME AND BOX PROCESSING TIME FOR WEBPA SET OPERATION WITH SINGLE PARAM AND DIFFERENT VALUES FOR 10 ITERATION");
			LOGGER.info("**********************************************************************************\n");

			LOGGER.info("*****************************************************************************************");
			LOGGER.info(
					"ITERATION STEPS: DESCRIPTION : VERIFY THE WEBPA SET OPERATION IS SUCCESSFUL, ALSO END TO END & BOX PROCESSING TIME IS CALCULATED");
			LOGGER.info("ITERATION STEPS: ACTION : Execute WEBPA set request");
			LOGGER.info(
					"ITERATION STEPS: EXPECTED : WEBPA SET OPERATION SHOULD BE SUCCESSFUL AND END TO END & BOX PROCESSING TIME SHOULD BE OBTAINED");
			LOGGER.info("*****************************************************************************************");

			// Checking atom sync to verify WEBPA logs in atom console for Atom based
			// devices
			boolean isAtom = CommonMethods.isAtomSyncAvailable(device, tapEnv);

			for (int count = 0; count < WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_10; count++) {

				iteration++;

				LOGGER.info("**********************************************************************************");
				LOGGER.info("ITERATION :" + iteration
						+ ": VERIFY THE WEBPA SET OPERATION IS SUCCESSFUL, ALSO END TO END & BOX PROCESSING TIME IS CALCULATED");
				LOGGER.info("**********************************************************************************");

				List<Long> result = new BroadBandWebPaUtils().calculateEndToEndAndBoxProcessingTimeForWebPaOperation(
						tapEnv, device, isAtom, true, iteration);

				endToEndProcessingTime = result.get(BroadBandTestConstants.CONSTANT_0);
				boxProcessingTime = result.get(BroadBandTestConstants.CONSTANT_1);

				if (endToEndProcessingTime != 0 && boxProcessingTime != 0) {
					successCount++;
					collectiveTime += endToEndProcessingTime;
					collectiveBoxProcessingTime += boxProcessingTime;

					LOGGER.info("**********************************************************************************");
					LOGGER.info("WEBPA SET REQUEST BOX PROCESSING TIME IN MILLISECONDS FOR INTERATION " + iteration
							+ " : " + boxProcessingTime / 1000000.0);
					LOGGER.info("END TO END WEBPA SET REQUEST PROCESSING TIME IN MILLISECONDS FOR INTERATION "
							+ iteration + " : " + endToEndProcessingTime);
					LOGGER.info("**********************************************************************************\n");

				} else {
					LOGGER.error(
							"UNABLE TO CALCULATE END TO END/BOX PROCESSING TIME FOR WEBPA GET EXECUTION WITH SINGLE PARAM");
				}
				tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
			}

			testStep = "s1";
			status = false;
			errorMessage = "SUCCESS RATE IS NOT 100%. EXPECTED SUCCESS COUNT : 10; ACTUAL SUCCESS COUNT : "
					+ successCount;

			LOGGER.info("*****************************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : CALCULATE SUCCESS RATE FOR WEBPA SET REQUEST WITH SINGLE PARAMETER");
			LOGGER.info("STEP 1: ACTION : Check success rate for WEBPA request with Set parameters");
			LOGGER.info(
					"STEP 1: EXPECTED : SUCCESS RATE FOR WEBPA SET REQUEST WITH ONE PARAMETER IN 10 ITERATION SHOULD BE 10/10 RATE");
			LOGGER.info("*****************************************************************************************");

			status = (successCount == BroadBandTestConstants.CONSTANT_10);
			LOGGER.info(
					"SUCCESS COUNT FOR WEBPA SET OPERATION WITH SINGLE PARAM OUT OF 10 ITERATION IS : " + successCount);
			if (status) {
				LOGGER.info(
						"STEP 1: ACTUAL: SUCCESS RATE FOR WEBPA GET OPERATION WITH SINGLE PARAM FOR 10 ITERATION IS 100 % : "
								+ successCount + "/" + WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_10);
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

			testStep = "s2";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION: CALCULATE AVERAGE END TO END WEBPA REQUEST PROCESSING TIME FOR WEBPA SET REQUEST WITH SINGLE PARAMETER");
			LOGGER.info(
					"STEP 2: EXPECTED: AVERAGE RESPONSE TIME FOR WEBPA SET REQUEST WITH SINGLE PARAMETER SHOULD BE LESS THAN 6000 MILLISECONDS");
			LOGGER.info("**********************************************************************************");
			meanAverageTime = collectiveTime / successCount;
			LOGGER.info("AVERAGE END TO END WEBPA SET REQUEST PROCESSING TIME : " + meanAverageTime + " MILLISECONDS");
			errorMessage = "AVERAGE RESPONSE TIME FOR WEBPA REQUEST WITH ONE SET PARAMETER IS GREATER THAN 6000 MILLISECONDS. ACTUAL : "
					+ meanAverageTime + " msec";
			status = meanAverageTime <= 6000;
			if (status) {
				LOGGER.info("STEP 2: ACTUAL: AVERAGE RESPONSE TIME FOR WEBPA SET REQUEST WITH SINGLE PARAMETER FOR "
						+ successCount + " SUCCESSFUL ITERATIONS IS :" + meanAverageTime + " MILLISECONDS");
			} else {
				LOGGER.error("STEP 2:ACTUAL:" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

			LOGGER.info("##########################################################################");
			LOGGER.info("AVERAGE END TO END WEBPA SET REQUEST PROCESSING TIME WITH SINGLE PARAM FOR " + successCount
					+ " ITERATION : " + meanAverageTime + " MILLISECONDS");
			LOGGER.info("AVERAGE BOX PROCESSING TIME FOR WEBPA SET REQUEST WITH SINGLE PARAM FOR " + successCount
					+ " ITERATION : " + ((collectiveBoxProcessingTime / successCount) / 1000000.0) + " MILLISECONDS");
			LOGGER.info("##########################################################################");

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, false);
		} finally {

			LOGGER.info("\n**********************************************************************************");
			LOGGER.info("##########################  STARTING POST-CONFIGURATIONS ##########################");
			LOGGER.info("**********************************************************************************");
			LOGGER.info("POST-CONDITION : DESCRIPTION : REVERT THE WEBPA PARAM USED IN THIS TEST TO ITS DEFAULT VALUE");
			LOGGER.info(
					"POST-CONDITION: EXPECTED : WEBPA PARAM USED IN THIS TEST SHOULD BE SUCCESSFULLY UPDATED WITH ITS DEFAULT VALUE");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_SELFHEAL_PINGINTERVAL,
					BroadBandTestConstants.CONSTANT_2, BroadBandTestConstants.STRING_VALUE_15);
			if (status) {
				LOGGER.info("WEBPA PARAM USED IN THIS TEST IS SUCCESSFULLY UPDATED WITH ITS DEFAULT VALUE");
			} else {
				LOGGER.error("UNABLE TO REVERT THE WEBPA PARAM TO ITS DEFAULT VALUE");
			}
			LOGGER.info("**********************************************************************************");
			LOGGER.info("#########################  COMPLETED POST-CONFIGURATIONS #########################");
			LOGGER.info("**********************************************************************************");

		}

		LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-WEBPA-PERF-1003 ####################");
	}

	/**
	 * Calculate Average Response Time for WEBPA request with Five Set parameters
	 * <ol>
	 * <li>PRE-CONDITION :Check whether WebPA is Up and Running.</li>
	 * <li>ITERATION STEP 1:Verify webPA set response from device with five
	 * parameters .</li>
	 * <li>Repeat the ITERATION STEP 1 for ten times totally 10 steps</li>
	 * <li>STEP 1: Calculate Success rate for WEBPA request with Five Set
	 * parameters.</li>
	 * <li>STEP 2: Calculate Average Response Time for WEBPA request with Five Set
	 * parameters.</li>
	 * <li>POST-CONDITION :Factory reset device and reactivate it</li>
	 * </ol>
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WEBPA-PERF-1004")
	public void wepaPerformanceTestWithFiveSetParameters(Dut device) {

		boolean status = false;// boolean to store the test case status
		String testId = "TC-RDKB-WEBPA-PERF-104";// Test case id
		String testStep = null;// Test step number
		String errorMessage = null;// String to store the error message
		Integer successCount = 0;// Integer to store successcount
		long collectiveTime = 0;// Long to add averagetime on every iteration
		long timeDifference = 0;// long to store time difference
		long meanAverageTime = 0;// Long to store averagetime
		long expectedMeanAvgTime = 6000;
		Integer iteration = 0;// Map to store response
		// Webpaparamter list store parameter objects
		List<WebPaParameter> webpaSetParameters = new ArrayList<>();
		long expectedMeanAvgTimeForAtombasedDevice = 10000;
		try {
			LOGGER.info("#################### STARTING TEST CASE:TC-RDKB-WEBPA-PERF-1004 #####################");
			LOGGER.info("TEST DESCRIPTION: Calculate Average Response Time for WEBPA request with Five Set parameters");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION :Check whether Webpa is Up and Running");
			LOGGER.info("ITERATION STEPs : Verify webpa set response from device with ten parameters");
			LOGGER.info("Repeat the ITERATION STEP 1 for ten times totally 10 steps.");
			LOGGER.info("1: Calculate Success rate for WEBPA request with ten get parameters ");
			LOGGER.info("2: Calculate Average Response Time for WEBPA request with ten get parameters ");
			LOGGER.info("#####################################################################################");

			LOGGER.info("####################################STARTING PRE-CONFIGURATIONS############################");
			LOGGER.info("PRECONDITION: DESCRIPTION: VERIFY WHETHER WEBPA IS UP AND RUNNING");
			LOGGER.info(
					"PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE ,IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
			LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			errorMessage = "Webpa is not Up and Running";
			if (!status) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			LOGGER.info("###############################COMPLETED PRE-CONFIGURATIONS###############################");

			LOGGER.info("##########################################################################");
			LOGGER.info("Calculate Average Response Time for WEBPA request with Five Set parameter");
			LOGGER.info("##########################################################################");
			LOGGER.info("ITERATION STEP 1:Verify webpa set response from device with five parameters ");
			LOGGER.info("EXPECTED:Webpa set response from device should be successful");
			LOGGER.info("##########################################################################");

			for (int count = 0; count < WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_5; count++) {
				iteration = count + 1;
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				LOGGER.info("##########################################################################");
				LOGGER.info(" ITERATION STEP " + iteration
						+ ": Verify webpa set response from device with five parameters ");
				webpaSetParameters = BroadBandWebPaUtils.webpaSetFiveParameter(count);
				LOGGER.info("##########################################################################");
				LOGGER.info("WEBPA PARAMETERS AND VALUES USED IN FOLLOWING EXECUTION");
				for (int countin = 0; countin < webpaSetParameters.size(); countin++) {
					LOGGER.info("WEBPA PARAMETER: " + webpaSetParameters.get(countin).getName() + " PARAMETER VALUE: "
							+ webpaSetParameters.get(countin).getValue() + " SET IN FOLLOWING EXECUTION");
				}
				LOGGER.info("##########################################################################");

				long endTime = 0;
				long startTime = 0;
				int returnCode = 0;
				WebPaServerResponse webPaServerResponse = new WebPaServerResponse();
				// Execution of webPA set command
				startTime = System.currentTimeMillis();
				webPaServerResponse = WebPaConnectionHandler.get().setWebPaParameterValue(device, webpaSetParameters);
				endTime = System.currentTimeMillis();

				returnCode = webPaServerResponse.getStatusCode();
				if (returnCode == 200) {
					LOGGER.info(
							"Verification of webpa get response from device is Success in iteration step " + iteration);
					timeDifference = endTime - startTime;
				} else {
					LOGGER.info(
							"Verification of webpa get response from device is Failure in iteration step " + iteration);
				}
				if (timeDifference != 0) {
					successCount++;
					collectiveTime += timeDifference;
				} else {
					LOGGER.error("Failure in capturing time difference of webpa execution");
				}
			}

			/**
			 * STEP 1: Calculate Success rate for WEBPA request with Five Set parameters
			 * Request
			 */
			testStep = "s1";
			status = false;
			LOGGER.info("****************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION: Calculate Success rate for WEBPA request with Five Set parameters Request");
			LOGGER.info("STEP 1: ACTION: Check the Success rate for WEBPA request ");
			LOGGER.info(
					"STEP 1: EXPECTED: Success rate for WEBPA request with Five Set parameters should be 10/10 rate");
			LOGGER.info("****************************************************************");
			errorMessage = "Success rate is Zero,reason might be webpa is not responding";
			if (successCount != 0) {
				status = true;
				LOGGER.info("STEP 1:ACTUAL:Success rate for WEBPA request with Five Set parameters is :" + successCount
						+ "/" + WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_5);
			} else {
				LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
			}
			LOGGER.info("****************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

			/**
			 * STEP 2: Calculate Average Response Time for webPA request with Five Set
			 * parameters
			 */
			testStep = "s2";
			status = false;
			LOGGER.info("****************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION: Calculate Average Response Time for WEBPA request with Five Set parameters");
			LOGGER.info("STEP 2: ACTION: Check the value with the average mean time ");
			LOGGER.info(
					"STEP 2: EXPECTED: Average Response Time for WEBPA request with Five Set parameters Request should be within the limits eg. less than 10000msec for Atom ");
			LOGGER.info("****************************************************************");
			meanAverageTime = collectiveTime / successCount;
			errorMessage = "Average Response Time for WEBPA request with Five Set parameters : ACTUAL RESPONSE: "
					+ meanAverageTime + "msec";
			
			try {
				
				String expectedMeanAvgTimeInString=BroadBandCommonUtils.getAutomaticsPropsValueByResolvingPlatform(device,  BroadBandTestConstants.PROP_KEY_AVG_RESPONSE_TIME);
				expectedMeanAvgTime=Integer.valueOf(expectedMeanAvgTimeInString);
			} catch (Exception e) {
				expectedMeanAvgTime=60000;
			    LOGGER.info("Average Response Time for WEBPA request taking as 60000 as no device specific value found");
			}
			
			if (meanAverageTime <= expectedMeanAvgTime) {
				LOGGER.info("STEP 2:ACTUAL:Average Response Time for WEBPA request with Five Set parameters for "
						+ successCount + " Successful iterations is :" + meanAverageTime + "msec");
			} else {
				LOGGER.error(
						"Average Response Time is far higher than expected range. Please cross verify and create defect if required"
								+ errorMessage);
			}
			status = true;
			LOGGER.info("****************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Faliure in executing Webpa set command with five parameters \n" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
		} finally {
			LOGGER.info("##########################################################################");
			LOGGER.info("Average Response Time for WEBPA request with Five Set parameters:" + meanAverageTime);
			LOGGER.info("##########################################################################");
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION : DESCRIPTION : FACTORY RESET DEVICE.");
			LOGGER.info("POST-CONDITION : ACTION : DEVICE SHOULD FACTORY RESET SUCCESSFULLY. ");
			LOGGER.info("POST-CONDITION : EXPECTED : DEVICE SHOULD FACTORY RESET");
			LOGGER.info("### POST-CONDITION ### BEGIN FACTORY RESET");
			boolean isFactoryReset = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
			LOGGER.info("### POST-CONDITION ### END FACTORY RESET.");
			if (isFactoryReset) {
				LOGGER.info("POST-CONDITION : DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
				LOGGER.info("POST-CONDITION : ACTION : BROAD BAND DEVICE REACTIVATION. ");
				LOGGER.info("POST-CONDITION : EXPECTED : device should get reactivated");
				LOGGER.info("### POST-CONDITION ### BEGIN BROAD BAND DEVICE REACTIVATION.");
				BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
				LOGGER.info("### POST-CONDITION ### END BROAD BAND DEVICE REACTIVATION.");
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			}
		}
		LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-WEBPA-PERF-1004 ####################");
	}

	/**
	 * Calculate Average Response Time for WEBPA request with Ten Set parameters
	 * <ol>
	 * <li>PRE-CONDITION :Check whether WebPA is Up and Running.</li>
	 * <li>ITERATION STEP 1:Verify webPA set response from device with Ten
	 * parameters</li>
	 * <li>Repeat the ITERATION STEP 1 ten times totally 10 steps</li>
	 * <li>STEP 1: Calculate Success rate for webPA request with Ten Set
	 * parameters.</li>
	 * <li>STEP 2: Calculate Average Response Time for WEBPA request with Ten Set
	 * parameters.</li>
	 * <li>POST-CONDITION :Factory reset device and reactivate it</li>
	 * </ol>
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
			TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WEBPA-PERF-1005")
	public void wepaPerformanceTestWithTenSetParameters(Dut device) {

		boolean status = false;// boolean to store the test case status
		String testId = "TC-RDKB-WEBPA-PERF-105";// Test case id
		String testStep = null;// Test step number
		String errorMessage = null;// String to store the error message
		Integer successCount = 0;// Integer to store successcount
		long collectiveTime = 0;// Long to add averagetime on every iteration
		long meanAverageTime = 0;// Long to store averagetime
		Integer iteration = 0;// Map to store response
		long timeDifference = 0;// long to store time difference
		long expectedMeanAvgTime = 6000;
		// Webpaparameter list to store webpaparamter objects
		List<WebPaParameter> webpaSetParameters = new ArrayList<>();
		long expectedMeanAvgTimeForAtombasedDevice = 10000;
		String successMessage = null;
		try {
			LOGGER.info("#################### STARTING TEST CASE:TC-RDKB-WEBPA-PERF-1005 #####################");
			LOGGER.info("TEST DESCRIPTION: Calculate Average Response Time for WEBPA request with Ten Set parameters");
			LOGGER.info("TEST STEPS : ");
			LOGGER.info("PRE-CONDITION :Check whether Webpa is Up and Running");
			LOGGER.info("ITERATION STEP 1:Verify webpa set response from device with ten parameters");
			LOGGER.info("Repeat the ITERATION STEP 1 for ten times totally 10 steps");
			LOGGER.info("1: Calculate Success rate for WEBPA request with Ten Set parameters ");
			LOGGER.info("2: Calculate Average Response Time for WEBPA request with Ten Set parameters ");
			LOGGER.info("#####################################################################################");
			LOGGER.info("####################################STARTING PRE-CONFIGURATIONS############################");
			LOGGER.info("##########################################################################");
			LOGGER.info("PRECONDITION: DESCRIPTION: VERIFY WHETHER WEBPA IS UP AND RUNNING");
			LOGGER.info(
					"PRECONDITION: ACTION : VERIFYING SUCCESSFUL WEBPA GET RESPONSE, IN CASE OF FAILURE RECHECKING FOR 8 MINUTES");
			LOGGER.info("PRECONDITION: EXPECTED : WEBPA PROCESS SHOULD BE UP AND RUNNING");
			LOGGER.info("##########################################################################");
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			errorMessage = "Webpa is not Up and Running";
			if (!status) {
				throw new TestException(RDKBTestConstants.PRE_CONDITION_ERROR + errorMessage);
			}
			LOGGER.info("###############################COMPLETED PRE-CONFIGURATIONS###############################");
			LOGGER.info("##########################################################################");
			LOGGER.info("Calculate Average Response Time for WEBPA request with Ten Set parameters");
			LOGGER.info("##########################################################################");

			LOGGER.info("*****************************************************************************************");
			LOGGER.info("ITERATION STEPS: DESCRIPTION : Verify webpa set response from device with Ten parameters");
			LOGGER.info("ITERATION STEPS: ACTION : Execute WEBPA set request with Ten parameters");
			LOGGER.info("ITERATION STEPS: EXPECTED : Webpa set response from device should be successful");
			LOGGER.info("*****************************************************************************************");

			for (int count = 0; count < WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_10; count++) {
				iteration = count + 1;
				tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
				LOGGER.info("##########################################################################");
				LOGGER.info("ITERATION " + iteration
						+ " ITERATION STEP 1:Verify webpa set response from device with ten parameters ");
				if (!DeviceModeHandler.isDSLDevice(device)) {
					webpaSetParameters = BroadBandWebPaUtils.webpaSetTenParameter(count);
				} else {
					webpaSetParameters = BroadBandWebPaUtils.webpaSetTenParametersForDSL(count);
				}
				LOGGER.info("##########################################################################");
				LOGGER.info("WEBPA PARAMETERS AND VALUES USED IN ITERATION STEP : " + iteration);
				for (int countin = 0; countin < webpaSetParameters.size(); countin++) {
					LOGGER.info("WEBPA PARAMETER: " + webpaSetParameters.get(countin).getName() + " PARAMETER VALUE: "
							+ webpaSetParameters.get(countin).getValue() + " SET IN FOLLOWING EXECUTION");
				}
				LOGGER.info("##########################################################################");

				long endTime = 0;
				long startTime = 0;
				int code = 0;
				WebPaServerResponse webPaServerResponse = new WebPaServerResponse();
				// Execution of webPA set command
				startTime = System.currentTimeMillis();
				webPaServerResponse = WebPaConnectionHandler.get().setWebPaParameterValue(device, webpaSetParameters);
				endTime = System.currentTimeMillis();

				code = webPaServerResponse.getStatusCode();
				if (code == 200) {
					LOGGER.info(
							"Verification of webpa get response from device is Success in iteration step " + iteration);
					timeDifference = endTime - startTime;
				} else {
					LOGGER.info(
							"Verification of webpa get response from device is Failure in iteration step " + iteration);
				}
				if (timeDifference != 0) {
					successCount++;
					collectiveTime += timeDifference;
					LOGGER.info(
							"collectiveTime in iteration step " + iteration+"is "+ collectiveTime);
				} else {
					LOGGER.error("Failure in capturing time difference of webpa execution");
				}
			}
			/**
			 * STEP 1: Calculate Success rate for WEBPA request with Ten Set parameters
			 */
			testStep = "s1";
			status = false;
			errorMessage = "Success rate is Zero,reason might be webpa is not responding";

			LOGGER.info("*****************************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Calculate Success rate for WEBPA request with Ten Set parameters Request");
			LOGGER.info("STEP 1: ACTION : Check the Success rate for WEBPA request ");
			LOGGER.info(
					"STEP 1: EXPECTED : Success rate for WEBPA request with Ten Set parameters should be 10/10 rate");
			LOGGER.info("*****************************************************************************************");

			if (successCount != 0) {
				status = true;
				LOGGER.info("STEP 1:ACTUAL:Success rate for WEBPA request with Ten Set parameters is :" + successCount
						+ "/" + WEBPA_PERFORMANCE_NUMBER_OF_ITERATION_10);
			} else {
				LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
			}
			LOGGER.info("****************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
			/**
			 * STEP 2: Calculate Average Response Time for WEBPA request with Ten Set
			 * parameters
			 */
			testStep = "s2";
			status = false;
			errorMessage = successMessage = "Average Response Time for WEBPA request with Ten Set parameters : ACTUAL RESPONSE: "
					+ meanAverageTime + "msec";

			LOGGER.info("*****************************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Calculate Average Response Time for WEBPA request with Ten Set parameters");
			LOGGER.info("STEP 2: ACTION : Check the value with the average mean time");
			LOGGER.info(
					"STEP 2: EXPECTED : Average Response Time for WEBPA request with Ten Set parameters Request should be less than 10000msec for atom based devices 6000msec for others");
			LOGGER.info("*****************************************************************************************");

			meanAverageTime = collectiveTime / successCount;
			try {
				
				String expectedMeanAvgTimeInString=BroadBandCommonUtils.getAutomaticsPropsValueByResolvingPlatform(device, BroadBandTestConstants.PROP_KEY_AVG_RESPONSE_TIME);
				expectedMeanAvgTime=Integer.valueOf(expectedMeanAvgTimeInString);
			} catch (Exception e) {
				expectedMeanAvgTime=60000;
			    LOGGER.info("Average Response Time for WEBPA request taking as 60000 as no device specific value found");
			}

			/*expectedMeanAvgTime = CommonMethods.isAtomSyncAvailable(device, tapEnv)
					? expectedMeanAvgTimeForAtombasedDevice
					: expectedMeanAvgTime;*/

			if (meanAverageTime <= expectedMeanAvgTime) {
				status = true;
				successMessage += "Average Response Time Received is less than the expected Average Time "
						+ expectedMeanAvgTime + "msec";
			} else {
				errorMessage += "Average Response Time is far higher than expected range. Please cross verify and create defect if required";
			}

			if (status) {
				LOGGER.info("STEP 2: ACTUAL :" + successMessage);
			} else {
				LOGGER.info("STEP 2: ACTUAL :" + errorMessage);
			}
			LOGGER.info("****************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Faliure in executing Webpa set command with ten parameters \n" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
		} finally {
			LOGGER.info("##########################################################################");
			LOGGER.info("Average Response Time for WEBPA request with Ten Set parameters:" + meanAverageTime + "msec");
			LOGGER.info("##########################################################################");
			LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
			LOGGER.info("POST-CONDITION : DESCRIPTION : FACTORY RESET DEVICE.");
			LOGGER.info("POST-CONDITION : ACTION : DEVICE SHOULD FACTORY RESET SUCCESSFULLY. ");
			LOGGER.info("POST-CONDITION : EXPECTED : DEVICE SHOULD FACTORY RESET");
			LOGGER.info("### POST-CONDITION ### BEGIN FACTORY RESET");
			boolean isFactoryReset = false;

			try {

				isFactoryReset = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
				LOGGER.info("### POST-CONDITION ### END FACTORY RESET.");
			} catch (Exception e) {
				isFactoryReset = false;
				LOGGER.error("### POST-CONDITION ### Exception while performing factory reset." + e.getMessage());
			}

			if (isFactoryReset) {

				LOGGER.info("STEP 1: DESCRIPTION : BEGIN BROAD BAND DEVICE REACTIVATION.");
				LOGGER.info("STEP 1: ACTION : BROAD BAND DEVICE REACTIVATION. ");
				LOGGER.info("STEP 1: EXPECTED : device should get reactivated");
				LOGGER.info(
						"*****************************************************************************************");

				LOGGER.info("### POST-CONDITION ### BEGIN BROAD BAND DEVICE REACTIVATION.");
				BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
				LOGGER.info("### POST-CONDITION ### END BROAD BAND DEVICE REACTIVATION.");
				LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
			} else {
				BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
			}
		}
		LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-WEBPA-PERF-1005 ####################");
	}
	
    /**
     * 
     * Calculate Average Response Time for two,three and four Get parameters*
     * <ol>
     * <li>PRE-CONDITION: Check whether Webpa is Up and Running.</li>*
     * <li>STEP 1: a)Verify webpa get response from device with two parameters
     *                 b)Repeat the "ITERATION STEP 1: a" for ten times totally 10 steps
     *                 c)Calculate Success rate for WEBPA request with two Get parameters.</li>
     * <li>STEP 2: Calculate Average Response Time for WEBPA request with two Get parameters.</li>
     * <li>STEP 3: a)Verify webpa get response from device with three parameters
     *                 b)Repeat the "ITERATION STEP 3: a" for ten times totally 10 steps
     *                 c)Calculate Success rate for WEBPA request with three Get parameters.</li>
     * <li>STEP 4: Calculate Average Response Time for WEBPA request with three Get parameters.</li>
     * <li>STEP 5: a)Verify webpa get response from device with four parameters
     *                 b)Repeat the "ITERATION STEP 5: a" for ten times totally 10 steps
     *                 c)Calculate Success rate for WEBPA request with four Get parameters.</li>
     * <li>STEP 6: Calculate Average Response Time for WEBPA request with four Get parameters.</li>
     * </ol>
     * @author Deepika
     * @Refactor Sruthi Santhosh
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, groups = {
           BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-WEBPA-PERF-1006")
    public void wepaPerformanceTestWithGetParamters(Dut device) {
       boolean status = false;// boolean to store the test case status
       String testId = "TC-RDKB-WEBPA-PERF-106";// Test case id
       String testStep = null;// Test step number
       String errorMessage = null;// String to store the error message
       Integer successCount = 0;// Integer to successcount
       long collectiveTime = 0;// Long to store addition of time difference
       long averageTime = 0;// Long to Average response time
       try {
           LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-WEBPA-PERF-1006 #####################");
           LOGGER.info("TEST DESCRIPTION:Calculate Average Response Time for WEBPA request with five Get parameters ");
           LOGGER.info("TEST STEPS : ");
           LOGGER.info("PRE-CONDITION :Check whether Webpa is Up and Running");
           LOGGER.info("STEP 1:a)Verify webpa get response from device with two parameters\r\n"
                  + "             b)Repeat the \"ITERATION STEP 1: a\" for ten times totally 10 steps\r\n"
                  + "             c)Calculate Success rate for WEBPA request with two Get parameters.");
           LOGGER.info("STEP 2:Calculate Average Response Time for WEBPA request with two Get parameters ");
           LOGGER.info("STEP 3:a)Verify webpa get response from device with three parameters\r\n"
                  + "      b)Repeat the \"ITERATION STEP 3: a\" for ten times totally 10 steps\r\n"
                  + "      c)Calculate Success rate for WEBPA request with three Get parameters.");
           LOGGER.info("STEP 4:Calculate Average Response Time for WEBPA request with three Get parameters ");
           LOGGER.info("STEP 5:a)Verify webpa get response from device with four parameters\r\n"
                  + "      b)Repeat the \"ITERATION STEP 5: a\" for ten times totally 10 steps\r\n"
                  + "      c)Calculate Success rate for WEBPA request with four Get parameters.");
           LOGGER.info("STEP 6:Calculate Average Response Time for WEBPA request with four Get parameters ");
           LOGGER.info("#####################################################################################");

           LOGGER.info("####################################STARTING PRE-CONFIGURATIONS############################");
           LOGGER.info("PRECONDITION:Verify whether Webpa is Up and Running");
           LOGGER.info(
                  "DESCRIPTION:Verifying Successful webpa Get response ,in case of failure rechecking for 8 minutes");
           LOGGER.info("##########################################################################");
           status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
           errorMessage = "Webpa is not Up and Running";
           if (!status) {
              throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
           }
           LOGGER.info("PRE-CONDITION  : ACTUAL : WEBPA IS UP AND RUNNING");
           LOGGER.info("###############################COMPLETED PRE-CONFIGURATIONS###############################");

           /**
            * STEP 1:a)Verify webpa get response from device with two parameters\r\n + b)Repeat the "ITERATION STEP 1:
            * a" for ten times totally 10 steps\r\n + c)Calculate Success rate for WEBPA request with two Get
            * parameters.
            */
           testStep = "s1";
           status = false;
           LOGGER.info("**********************************************************************************");
           LOGGER.info("STEP 1:DESCRIPTION:Calculate Success rate for WEBPA request with two Get parameters");
           LOGGER.info("STEP 1:ACTION:a)Verify webpa get response from device with two parameters\\r\\n\" + \r\n"
                  + "                    b)Repeat it for ten times totally 10 steps\\r\\n\"");
           LOGGER.info("STEP 1:EXPECTED: Success rate for WEBPA request with two Get parameters should be 10/10 rate");
           LOGGER.info("**********************************************************************************");
           errorMessage = "Success rate is Zero,reason might be webpa is not responding";
           String[] webpaGetParameters = BroadBandWebPaUtils.webpaGetParameters(2);
           successCount = wepaPerformanceIterationTest(webpaGetParameters, device, testStep);
           status=(successCount != 0);
           if(status) { 
              LOGGER.info("STEP 1: ACTUAL:Success rate for WEBPA request with two Get parameters " + successCount
                     + "/" + WEBPA_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION);
           } else {
              LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
           }
           LOGGER.info("**********************************************************************************");
           tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

           /**
            * STEP 2: Calculate Average Response Time for WEBPA request with two Get parameters
            */
           testStep = "s2";
           status = false;
           LOGGER.info("**********************************************************************************");
           LOGGER.info("STEP 2:DESCRIPTION:Calculate Average Response Time for WEBPA request with two Get parameters");
           LOGGER.info("STEP 2:ACTION:CollectiveTime/Successcount should be less than 1000msec");
           LOGGER.info(
                  "STEP 2:EXPECTED: Average Response Time for WEBPA request with two Get parameters should be less than 1000msec");
           LOGGER.info("**********************************************************************************");
           averageTime = collectiveTime / successCount;
           errorMessage = "Average Response Time for WEBPA request with two Get parameters is greater than 1000msec. ACTUAL RESPONSE : "
                  + averageTime + " msec";
           status=(averageTime <= 1000);
                  if(status) {
              LOGGER.info("STEP 2:ACTUAL:Average Response Time for WEBPA request with two Get parameters for "
                     + successCount + " Successful iterations is :" + averageTime + "msec");
           } else {
              LOGGER.error("STEP 2:ACTUAL: " + errorMessage);
           }
           LOGGER.info("**********************************************************************************");
           tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
           /**
            * STEP 3:a)Verify webpa get response from device with two parameters\r\n + b)Repeat the "ITERATION STEP 3:
            * a" for ten times totally 10 steps\r\n + c)Calculate Success rate for WEBPA request with two Get
            * parameters.
            */
           testStep = "s3";
           status = false;
           LOGGER.info("**********************************************************************************");
           LOGGER.info("STEP 3:DESCRIPTION:Calculate Success rate for WEBPA request with three Get parameters");
           LOGGER.info("STEP 3:ACTION:a)Verify webpa get response from device with three parameters\\r\\n\" + \r\n"
                  + "                    b)Repeat it for ten times totally 10 steps\\r\\n\"");
           LOGGER.info(
                  "STEP 3:EXPECTED: Success rate for WEBPA request with three Get parameters should be 10/10 rate");
           LOGGER.info("**********************************************************************************");
           errorMessage = "Success rate is Zero,reason might be webpa is not responding";
           webpaGetParameters = BroadBandWebPaUtils.webpaGetParameters(3);
           successCount = wepaPerformanceIterationTest(webpaGetParameters, device, testStep);
           status=(successCount != 0);
           if(status) { 
              LOGGER.info("STEP 3: ACTUAL:Success rate for WEBPA request with three Get parameters " + successCount
                     + "/" + WEBPA_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION);
           } else {
              LOGGER.error("STEP 3:ACTUAL:" + errorMessage);
           }
           LOGGER.info("**********************************************************************************");
           tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

           /**
            * STEP 4: Calculate Average Response Time for WEBPA request with three Get parameters
            */
           testStep = "s4";
           status = false;
           LOGGER.info("**********************************************************************************");
           LOGGER.info(
                  "STEP 4:DESCRIPTION:Calculate Average Response Time for WEBPA request with three Get parameters");
           LOGGER.info("STEP 4:ACTION:CollectiveTime/Successcount should be less than 1000msec");
           LOGGER.info(
                  "STEP 4:EXPECTED: Average Response Time for WEBPA request with three Get parameters should be less than 1000msec");
           LOGGER.info("**********************************************************************************");
           averageTime = collectiveTime / successCount;
           errorMessage = "Average Response Time for WEBPA request with three Get parameters is greater than 1000msec. ACTUAL RESPONSE : "
                  + averageTime + " msec";
           status=(averageTime <= 1000);
           if(status) {
              LOGGER.info("STEP 4:ACTUAL:Average Response Time for WEBPA request with three Get parameters for "
                     + successCount + " Successful iterations is :" + averageTime + "msec");
           } else {
              LOGGER.error("STEP 4:ACTUAL: " + errorMessage);
           }
           LOGGER.info("**********************************************************************************");
           tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
           /**
            * STEP 5:a)Verify webpa get response from device with four parameters\r\n + b)Repeat the "ITERATION STEP 5:
            * a" for ten times totally 10 steps\r\n + c)Calculate Success rate for WEBPA request with four Get
            * parameters.
            */
           testStep = "s5";
           status = false;
           LOGGER.info("**********************************************************************************");
           LOGGER.info("STEP 5:DESCRIPTION:Calculate Success rate for WEBPA request with four Get parameters");
           LOGGER.info("STEP 5:ACTION:a)Verify webpa get response from device with four parameters\\r\\n\" + \r\n"
                  + "                    b)Repeat it for ten times totally 10 steps\\r\\n\"");
           LOGGER.info(
                  "STEP 5:EXPECTED: Success rate for WEBPA request with four Get parameters should be 10/10 rate");
           LOGGER.info("**********************************************************************************");
           errorMessage = "Success rate is Zero,reason might be webpa is not responding";
           webpaGetParameters = BroadBandWebPaUtils.webpaGetParameters(4);
           successCount = wepaPerformanceIterationTest(webpaGetParameters, device, testStep);
           status=(successCount != 0);
           if(status) {  
              LOGGER.info("STEP 5: ACTUAL:Success rate for WEBPA request with four Get parameters " + successCount
                     + "/" + WEBPA_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION);
           } else {
              LOGGER.error("STEP 5:ACTUAL:" + errorMessage);
           }
           LOGGER.info("**********************************************************************************");
           tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

           /**
            * STEP 6: Calculate Average Response Time for WEBPA request with four Get parameters
            */
           testStep = "s6";
           status = false;
           LOGGER.info("**********************************************************************************");
           LOGGER.info(
                  "STEP 6:DESCRIPTION:Calculate Average Response Time for WEBPA request with four Get parameters");
           LOGGER.info("STEP 6:ACTION:CollectiveTime/Successcount should be less than 1000msec");
           LOGGER.info(
                  "STEP 6:EXPECTED: Average Response Time for WEBPA request with four Get parameters should be less than 1000msec");
           LOGGER.info("**********************************************************************************");
           averageTime = collectiveTime / successCount;
           errorMessage = "Average Response Time for WEBPA request with three Get parameters is greater than 1000msec. ACTUAL RESPONSE : "
                  + averageTime + " msec";
           status=(averageTime <= 1000);
           if(status) {
              LOGGER.info("STEP 6:ACTUAL:Average Response Time for WEBPA request with four Get parameters for "
                     + successCount + " Successful iterations is :" + averageTime + "msec");
           } else {
              LOGGER.error("STEP 6:ACTUAL: " + errorMessage);
           }
           LOGGER.info("**********************************************************************************");
           tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
       } catch (Exception exception) {
           errorMessage = exception.getMessage();
           LOGGER.info("Failure in executing Webpa get command with two/three/four parameters \n" + errorMessage);
           CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
       }

    }

    /**
     * wepaPerformanceIterationTest method to perform webpa command to be iterated for 10 times
     * 
     * @param webpaGetParameters 
     *              Contains the webpa commands to be executed
     * @param device Dut
     * @param testStep 
     *              Contains the value of the test step being executed
     * @Refactor Sruthi Santhosh             
     */
    public Integer wepaPerformanceIterationTest(String[] webpaGetParameters, Dut device, String testStep) {
       Integer successCount = 0;// Integer to successcount
       long collectiveTime = 0;// Long to store addition of time difference
       Integer iteration = 0;// Integer to store iteration value
       long timeDifference = 0;// long to store time difference
       try {
       for (int count = 0; count < WEBPA_PERFORMANCE_MAXIMUM_NUMBER_OF_ITERATION; count++) {
           iteration = count + 1;
           LOGGER.info("**********************************************************************************");
           LOGGER.info("ITERATION :" + iteration + " of " + testStep
                  + ": Verify webpa get response from device with two parameters");
           timeDifference = BroadBandWebPaUtils.executeAndVerifyGetWebpaResponseReturnTimeDifference(webpaGetParameters, device);
           if (timeDifference != 0) {
              successCount++;
              collectiveTime += timeDifference;
           } else {
              LOGGER.error("Failure in capturing time difference of webpa execution");
           }
       }
       }catch (Exception exception) {
           LOGGER.info("Failure in executing Webpa get command iteration with two/three/four parameters \n" + exception.getMessage());
       }
       return successCount;
    }

    /**
     * Verify multiple webpa requests in parallel
     * <ol>
     * <li>PRE-CONDITION 1:Verify WebPa process is running in device</li>
     * <li>PRE-CONDITION 2:Verify parodus process is running</li>
     * <li>Step 1 :Verify parallel Webpa requests for multiple component webpa parameters</li>
     * </ol>
     * 
     * @author asanka200
     * @Refactor Alan_Bivera
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-WEBPA-PARALLEL-1006")
    public void testVerifyParallelWebPaRequests(Dut device) {

	// Variable Declaration begins
	// String to store test case id
	String testCaseId = "TC-RDKB-WEBPA-PARALLEL-006";
	// String to store step number
	String stepNum = "s1";
	// String to store error message
	String errorMessage = "";
	// boolean to store step result
	boolean status = false;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-PARALLEL-1006");
	LOGGER.info("TEST DESCRIPTION: Verify multiple webpa requests in parallel");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1:Verify WebPa process is running in device");
	LOGGER.info("PRE-CONDITION 2:Verify parodus process is running");
	LOGGER.info("1. Verify parallel webpa requests for multiple component webpa parameters");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Verify WebPa process is running in device");
	    LOGGER.info("PRE-CONDITION 1: ACTION : Execute command:pid of webpa");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED : Response should contain the process id of webpa");
	    LOGGER.info("#####################################################################");
	    long startTime = System.currentTimeMillis();
	    errorMessage = BroadBandTestConstants.PRE_CONDITION_ERROR + "Unable to verify pid of webpa process";
	    do {
		status = CommonMethods.isNotNull(BroadBandCommonUtils.getPidOfProcessResolvingArch(device, tapEnv,
			BroadBandTestConstants.PROCESS_NAME_WEBPA));
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("PRE-CONDITION 1: ACTUAL : Pid for Webpa process is obtained.");
	    } else {
		errorMessage = BroadBandTestConstants.PRE_CONDITION_ERROR + "Unable to verify pid of webpa process";
		LOGGER.error("PRE-CONDITION 1: ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("#####################################################################");
	    LOGGER.info("PRE-CONDITION 2 : DESCRIPTION : Verify parodus process is running");
	    LOGGER.info("PRE-CONDITION 2: ACTION : Execute command:pidof parodus");
	    LOGGER.info("PRE-CONDITION 2: EXPECTED : Pid for parodus process is obtained.");
	    LOGGER.info("#####################################################################");
	    errorMessage = BroadBandTestConstants.PRE_CONDITION_ERROR + "Unable to verify pid of parodus process";
	    status = CommonMethods.isNotNull(
		    CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_PARODUS));
	    if (status) {
		LOGGER.info("PRE-CONDITION 2: ACTUAL : Pid for parodus process is obtained.");
	    } else {
		errorMessage = BroadBandTestConstants.PRE_CONDITION_ERROR + "Unable to verify pid of parodus process";
		LOGGER.error("PRE-CONDITION 2: ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Unable to verify multiple webpa requests in parallel";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify parallel webpa requests for multiple component webpa parameters");
	    LOGGER.info("STEP 1: ACTION : Execute webpa requests at same time for multiple component webpa parameters");
	    LOGGER.info("STEP 1: EXPECTED : WebPA requests executed successfully");
	    LOGGER.info("**********************************************************************************");

	    List<String> paramList = new ArrayList<String>();
	    paramList.add(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID);
	    paramList.add(BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_FIRMWARE_NAME);
	    paramList.add(BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON);
	    paramList.add(BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_MANUFACTURER_INFO);

	    List<String> response = new ArrayList<String>();
	    response = BroadBandWebPaUtils.executeParallelWebPaRequests(device, tapEnv, paramList);
	    LOGGER.info("STEP 1: Response for parallel requests: " + response);
	    status = true;
	    for (String output : response) {
		status = status && CommonMethods.isNotNull(output);
	    }

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : WebPA requests executed successfully");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-PARALLEL-1006");
	LOGGER.info("#######################################################################################");
    }
  
   
}
