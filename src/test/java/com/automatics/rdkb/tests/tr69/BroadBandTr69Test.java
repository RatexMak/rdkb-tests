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

package com.automatics.rdkb.tests.tr69;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.TR69ParamDataType;
import com.automatics.providers.tr69.Parameter;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.TR69ParamConstants;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.tr69.BroadBandTr69Utils;

/**
 * Test class for validating TR-69 ACS related functionality.
 * 
 * @author Selvaraj Mariyappan
 *
 */
public class BroadBandTr69Test extends AutomaticsTestBase {
	/** Constant holds the test step number **/
	private static int stepNumber = 0;

	/**
	 * Test to Verify the Manufacturer serial number using TR69 parameter -
	 * Device.DeviceInfo.SerialNumber
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Selvaraj Mariyappan
	 * @refactor Rakesh C N
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-TR69-1000")
	public void testVerifyDeviceSerialNumberUsingTR69Parameter(Dut device) {

		String testCaseId = "TC-RDKB-TR69-001";
		String stepNumber = "s1";
		boolean status = false;
		String message = null;
		String tr69InitialStatus = null;

		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("PRE-CONDITION STEPS");
		tr69InitialStatus = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_ENABLECWMP);
		LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

		/**
		 * Step 1 : Retrieve the Serial number using TR-69 parameter
		 * Device.DeviceInfo.SerialNumber and verify with the actual serial number of
		 * the device using device object
		 */

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 1: Retrieve the Serial number  using TR-69 parameter 'Device.DeviceInfo.SerialNumber' "
				+ "and verify with the actual serial number of the device using device object");
		LOGGER.info("EXPECTED: Device Serial number should be retrieved successfully");
		LOGGER.info("**********************************************************************************");

		try {

			// Retrieve the serial number from the response
			String tr69DeviceSerialNumber = BroadBandTr69Utils.executeTr69ParameterCommand(tapEnv, device,
					TR69ParamConstants.TR69_PARAM_SERIAL_NUMBER);

			String manufacturerSerialNumber = device.getSerialNumber();
			message = " EXPECTED : Manufacturer Serial Number  : " + manufacturerSerialNumber;
			LOGGER.info("ACTUAL : Device Serial Number using TR-69 ACS : " + tr69DeviceSerialNumber + " , " + message);
			if (CommonMethods.isNotNull(tr69DeviceSerialNumber) && CommonMethods.isNotNull(manufacturerSerialNumber)) {
				// Verify with the actual serial number of the device
				status = (tr69DeviceSerialNumber.trim()).equalsIgnoreCase(manufacturerSerialNumber.trim());
				if (!status) {
					message = "Seems like 'Device.DeviceInfo.SerialNumber' providing wrong serial number . ACTUAL : Manufacture Serial number :  "
							+ tr69DeviceSerialNumber + message;
					LOGGER.error(message);
				}
			} else {
				status = false;
				message = "Unable to retrieve manufacturer serial number using TR69 parameter -'Device.DeviceInfo.SerialNumber'";
				LOGGER.error(message);
			}
			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, true);
		} catch (Exception exception) {
			status = false;
			message = "Unable to retrieve using TR69 parameter -'Device.DeviceInfo.SerialNumber'"
					+ exception.getMessage();
			LOGGER.error(message);
		} finally {
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1: DESCRIPTION : Revert the Inital TR069 Status");
			LOGGER.info("POST-CONDITION 1: ACTION : set Device.ManagementServer.EnableCWMP to Intial Status.");
			LOGGER.info("POST-CONDITION 1: EXPECTED : Reverting back the Tr069 to intial status should be successful.");
			LOGGER.info("#######################################################################################");
			status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_ENABLECWMP,
					BroadBandTestConstants.CONSTANT_3, tr69InitialStatus, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
					BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
			if (status) {
				LOGGER.info(
						"POST-CONDITION 1: ACTUAL : Reverting back the Tr069 to intial status should be successful.");
			} else {
				LOGGER.error(
						"POST-CONDITION 1: ACTUAL : Post condition failed : Reverting back the Tr069 to intial status is not successful.");
			}
			LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
			LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
		}
	}

	/**
	 * Test to Verify the device model name using TR69 parameter -
	 * Device.DeviceInfo.ModelName
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Selvaraj Mariyappan
	 * @refactor Rakesh C N
	 * 
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-TR69-1001")
	public void testVerifyDeviceModelNameUsingTR69Parameter(Dut device) {

		String testCaseId = "TC-RDKB-TR69-002";
		String stepNumber = "s1";
		boolean status = false;
		String message = null;

		/**
		 * Step 1 : Retrieve the Device Model Name using TR-69 parameter
		 * Device.DeviceInfo.ModelName and verify with the actual model name of the
		 * device using device object
		 */

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 1: Retrieve the device model name  using TR-69 parameter 'Device.DeviceInfo.ModelName' "
				+ "and verify with the actual model name of the device using device object");
		LOGGER.info("EXPECTED: Device model name should be retrieved successfully");
		LOGGER.info("**********************************************************************************");

		try {

			// Retrieve the device model name
			String tr69DeviceModelName = BroadBandTr69Utils.executeTr69ParameterCommand(tapEnv, device,
					TR69ParamConstants.TR69_PARAM_MODEL_NAME);
			String deviceModelName = device.getModel();
			message = " EXPECTED: Device model name  : " + deviceModelName;
			LOGGER.info("ACTUAL : Device model name using TR-69 ACS : " + tr69DeviceModelName + " , " + message);
			if (CommonMethods.isNotNull(tr69DeviceModelName) && CommonMethods.isNotNull(deviceModelName)) {
				// Verify with the actual model name of the device
				status = (tr69DeviceModelName.trim()).equalsIgnoreCase(deviceModelName.trim());
				if (!status) {
					message = "Seems like 'Device.DeviceInfo.ModelName' providing wrong model name . ACTUAL : Manufacture Device model name :  "
							+ tr69DeviceModelName + message;
					LOGGER.error(message);
				}
			} else {
				status = false;
				message = "Unable to retrieve the device model name using TR69 parameter -'Device.DeviceInfo.ModelName'";
				LOGGER.error(message);
			}
		} catch (Exception exception) {
			status = false;
			message = "Unable to retrieve using TR69 parameter -'Device.DeviceInfo.ModelName'" + exception.getMessage();
			LOGGER.error(message);
		} finally {

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, true);
		}
	}

	/**
	 * Test to Verify the current firmware version using TR69 parameter -
	 * Device.DeviceInfo.SoftwareVersion
	 * 
	 * @param device The device to be used.
	 * 
	 * @author Selvaraj Mariyappan
	 * @refactor Rakesh C N
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-TR69-1002")
	public void testVerifyDeviceSoftwareVersionUsingTR69Parameter(Dut device) {

		String testCaseId = "TC-RDKB-TR69-003";
		String stepNumber = "s1";
		boolean status = false;
		String message = null;

		/**
		 * Step 1 : Retrieve the Currently running firmware version using TR-69
		 * parameter Device.DeviceInfo.SoftwareVersion and verify with the actual
		 * currently running firmware version of the device using device object
		 */

		LOGGER.info("**********************************************************************************");
		LOGGER.info(
				"STEP 1: Retrieve the Currently running firmware version  using TR-69 parameter 'Device.DeviceInfo.SoftwareVersion' "
						+ "and verify with the actual firmware version of the device using device object");
		LOGGER.info("EXPECTED: Current software version should be retrieved successfully");
		LOGGER.info("**********************************************************************************");

		try {

			// Retrieve the current firmware version
			String tr69CurrentFirmwareVersion = BroadBandTr69Utils.executeTr69ParameterCommand(tapEnv, device,
					TR69ParamConstants.TR69_PARAM_SOFTWARE_VERSION);
			String firmwareVersion = BroadBandCommonUtils.getCurrentlyRunningImageVersionUsingWebPaCommand(tapEnv,
					device);
			message = " EXPECTED : Firmware version  : " + firmwareVersion;
			LOGGER.info("ACTUAL : Software version using TR-69 ACS : " + tr69CurrentFirmwareVersion + " , " + message);
			if (CommonMethods.isNotNull(tr69CurrentFirmwareVersion) && CommonMethods.isNotNull(firmwareVersion)) {
				// Verify with the actual firmware version of the device
				status = CommonMethods.patternMatcher(tr69CurrentFirmwareVersion, firmwareVersion);
				if (!status) {
					message = "Seems like 'Device.DeviceInfo.SoftwareVersion' providing wrong firmware version . ACTUAL :  Firmware version:  "
							+ tr69CurrentFirmwareVersion + message;
					LOGGER.error(message);
				}
			} else {
				status = false;
				message = "Unable to retrieve the Currently running firmware version using TR69 parameter -'Device.DeviceInfo.SoftwareVersion'";
				LOGGER.error(message);
			}
		} catch (Exception exception) {
			status = false;
			message = "Unable to retrieve the Currently running firmware version using TR69 parameter -'Device.DeviceInfo.SoftwareVersion'"
					+ exception.getMessage();
			LOGGER.error(message);
		} finally {

			tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, true);
		}
	}

	/**
	 * 
	 * This method verifies Advanced Management server parameters
	 * 
	 * <ol>
	 * <li>stepNumber 1: Execute get Parameter on Device.ManagementServer. to get
	 * the parameter values for Management of the device and validate the
	 * parameters</li>
	 * <li>stepNumber 2: Validate the Management Server URL</li>
	 * <li>stepNumber 3: Validate the Management Server Username</li>
	 * <li>stepNumber 4: Validate the Management Server Connection request URL</li>
	 * <li>stepNumber 5: Validate the Management Server Connection request
	 * Username</li>
	 * </ol>
	 * 
	 * @author Sathurya Ravi
	 * @refactor Athira
	 * 
	 * @param device Dut to be used for execution
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
	@TestDetails(testUID = "TC-RDKB-TR69-1010")

	public void testVerifyTR69AdvancedManagementServerParameters(Dut device) {
		// stores the test result
		boolean status = false;
		// stores the test case id
		String testId = "TC-RDKB-TR69-010";
		// stores the error message
		String errorMessage = null;
		// stores the stepNumber
		String stepNumber = null;
		// stores the response from dmcli command
		String response = "";
		// stores parameter list values
		String response1 = "";
		// stores parameter list values
		String response2 = "";
		// stores parameter list values
		String response3 = "";
		// stores parameter list values
		String response4 = "";
		// stores parameter list values
		List<String> parameterList = new ArrayList<String>();
		// stores value of parameter
		String parameterValue = "";
		// stores username array
		String username[] = { "", "" };

		try {
			/*
			 * stepNumber 1: Execute get Parameter on Advanced Management parameters of
			 * Device and get the response
			 * 
			 */

			LOGGER.info(
					"*****************************************************************************************************************************************************************");
			LOGGER.info("stepNumber 1: DESCRIPTION: Get the Management Server Parameters from TR69 ");
			LOGGER.info(
					"stepNumber 1: ACTION: Execute get Parameter on Device.ManagementServer.URL, Device.ManagementServer.Username, Device.ManagementServer.ConnectionRequestURL, Device.ManagementServer.ConnectionRequestUsername to get the parameter values  ");
			LOGGER.info("stepNumber 1: EXPECTED: The get Parameter should return response wihtout any errors ");
			LOGGER.info(
					"******************************************************************************************************************************************************************");

			stepNumber = "s1";
			status = false;

			List<String> parameters1 = new ArrayList<String>();
			parameters1.add(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_URL);

			List<String> parameters2 = new ArrayList<String>();
			parameters2.add(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_USERNAME);

			List<String> parameters3 = new ArrayList<String>();
			parameters3.add(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_CONNECTIONREQUESTURL);

			List<String> parameters4 = new ArrayList<String>();
			parameters4.add(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_CONNECTIONREQUESTUSERNAME);

			response1 = tapEnv.getTR69ParameterValues(device, parameters1);
			LOGGER.info("response of getTR69ParameterValues" + response1);
			response2 = tapEnv.getTR69ParameterValues(device, parameters2);
			LOGGER.info("response of getTR69ParameterValues" + response2);
			response3 = tapEnv.getTR69ParameterValues(device, parameters3);
			LOGGER.info("response of getTR69ParameterValues" + response3);
			response4 = tapEnv.getTR69ParameterValues(device, parameters4);
			LOGGER.info("response of getTR69ParameterValues" + response4);
			parameterList.add(response1);
			parameterList.add(response2);
			parameterList.add(response3);
			parameterList.add(response4);

			errorMessage = "Not able to get any parameter response for Management server or the Parameter values are not valid";

			status = parameterList != null && parameterList.size() > 0;
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : The get Parameters returns response wihtout any errors");
			} else {
				LOGGER.info("STEP 1: ACTUAL : " + errorMessage);

			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, true);

			/*
			 * stepNumber 2: Validate the Management Server URL
			 * 
			 */

			LOGGER.info(
					"*****************************************************************************************************************************************************************");
			LOGGER.info("stepNumber 2: DESCRIPTION: Validate the Management Server URL ");
			LOGGER.info("stepNumber 2: ACTION: Compare the value returned with the expected URL ");
			LOGGER.info("stepNumber 2: EXPECTED: The values should be the same ");
			LOGGER.info(
					"******************************************************************************************************************************************************************");

			stepNumber = "s2";
			status = false;

			parameterValue = parameterList.get(AutomaticsConstants.CONSTANT_0);
			LOGGER.info("STEP 2:  parameterValue " + parameterValue);
			errorMessage = "The Management server value is different from the actual value. Actual: " + parameterValue;
			status = parameterValue.equals(BroadBandTestConstants.MANAGEMENT_SERVER_URL);
			if (status) {
				LOGGER.info("STEP 2: ACTUAL : The values are same");
			} else {
				LOGGER.info("STEP 2: ACTUAL : " + errorMessage);

			}

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 3: Validate the Management Server Username
			 * 
			 */

			LOGGER.info(
					"*****************************************************************************************************************************************************************");
			LOGGER.info("stepNumber 3: DESCRIPTION: Validate the Management Server Username  ");
			LOGGER.info("stepNumber 3: ACTION: Compare the value returned with the expected Username ");
			LOGGER.info("stepNumber 3: EXPECTED: The values should be the same ");
			LOGGER.info(
					"******************************************************************************************************************************************************************");

			stepNumber = "s3";
			status = false;

			parameterValue = parameterList.get(AutomaticsConstants.CONSTANT_1);
			LOGGER.info("STEP 3:  parameterValue " + parameterValue);

			username = BroadBandTr69Utils.getUserNameForManagementServer(device);
			status = parameterValue.equals(username[1]);

			errorMessage = "The Management server value is different from the actual value. Actual: " + parameterValue;
			if (status) {
				LOGGER.info("STEP 3: ACTUAL : The values are same");
			} else {
				LOGGER.info("STEP 3: ACTUAL : " + errorMessage);

			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 4: Validate the Management Server Connection request URL
			 * 
			 */

			LOGGER.info(
					"*****************************************************************************************************************************************************************");
			LOGGER.info("stepNumber 4: DESCRIPTION: Validate the Management Server Connection request URL ");
			LOGGER.info("stepNumber 4: ACTION: Compare the value returned with the expected URL ");
			LOGGER.info("stepNumber 4: EXPECTED: The values should be the same ");
			LOGGER.info(
					"******************************************************************************************************************************************************************");

			stepNumber = "s4";
			status = false;

			parameterValue = parameterList.get(AutomaticsConstants.CONSTANT_2);
			LOGGER.info("STEP 4:  parameterValue " + parameterValue);
			response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_CONNECTIONREQUESTURL);
			LOGGER.info("STEP 4:  response " + response);
			if (null != response)
				status = parameterValue.equals(response);

			errorMessage = "The Management server connection request URL value is different from the actual value. Actual: "
					+ parameterValue;
			if (status) {
				LOGGER.info("STEP 4: ACTUAL : The values are same");
			} else {
				LOGGER.info("STEP 4: ACTUAL : " + errorMessage);

			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 5: Validate the Management Server Connection request Username
			 */

			LOGGER.info(
					"*****************************************************************************************************************************************************************");
			LOGGER.info("stepNumber 5: DESCRIPTION: Validate the Management Server Connection request Username ");
			LOGGER.info("stepNumber 5: ACTION: Compare the value returned with the expected URL ");
			LOGGER.info("stepNumber 5: EXPECTED: The values should be the same ");
			LOGGER.info(
					"******************************************************************************************************************************************************************");

			stepNumber = "s5";
			status = false;

			parameterValue = parameterList.get(AutomaticsConstants.CONSTANT_3);
			LOGGER.info("STEP 5:  parameterValue " + parameterValue);
			status = parameterValue.equals(username[0]);
			LOGGER.info("STEP 5:  username[0] " + username[0]);
			errorMessage = "The Management server username is different from the actual value. Actual: "
					+ parameterValue;
			if (status) {
				LOGGER.info("STEP 5: ACTUAL : The values are same");
			} else {
				LOGGER.info("STEP 5: ACTUAL : " + errorMessage);

			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception occured while validating Advanced Management Server parameters " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, errorMessage, true);
		}
	}

	/**
	 * 
	 * <ol>
	 * <li>PRE CONDITION 1:Verify the gateway has syndication partner</li>
	 * <li>1:Validate ACS Url in TR69 logs</li>
	 * <li>2:Perform Get operation using tr69.</li>
	 * <li>3:Perform Set operation using tr69</li>
	 * <li>4:Perform webpa command to validate ACS Management URL</li>
	 * <li>5:Perform Webpa command to validate ACS Management connection request
	 * port is not null</li>
	 * </ol>
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.TR69 })
	@TestDetails(testUID = "TC-RDKB-TR69-2001")
	public void testToVerifyACSandTr69(Dut device) {
		boolean status = false;// String to store the test case status
		String testId = "TC-RDKB-TR69-201";// Test case id
		String testStep = null;// Test step number
		String errorMessage = null;// String to store the error message
		String response = null;// String to store response.

		LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-TR69-2001#####################");
		LOGGER.info("TEST DESCRIPTION: Verify ACS related TR-69 functionality");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("PRE-CONDITION 1: Verify the gateway has syndication partner");
		LOGGER.info("1.Validate ACS Url in TR69 logs ");
		LOGGER.info("2.Perform Get operation using tr69.");
		LOGGER.info("3.Perform Set operation using tr69 ");
		LOGGER.info("4.Perform webpa command to validate ACS Management URL ");
		LOGGER.info("5.Perform Webpa command to validate ACS Management connection request port is not null");
		LOGGER.info("#######################################################################################");
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			BroadBandPreConditionUtils.executePreConditionToCheckSyndicationPartnerOnDevice(device, tapEnv,
					BroadBandTestConstants.CONSTANT_1);
			LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

			/**
			 * STEP 1:Validate ACS Url in TR69 logs
			 */
			status = false;
			testStep = "s1";
			errorMessage = "Unable validate ACS url from tr69 logs";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1 : DESCRIPTION :Validate ACS Url in TR69 logs ");
			LOGGER.info("STEP 1 : ACTION :Execute cat /rdklogs/logs/TR69log.txt.0 | grep cmconfig in device");
			LOGGER.info("STEP 1 : EXPECTED:ACS URL SHOULD BE AVAILABLE IN TR69 LOG ");
			LOGGER.info("**********************************************************************************");
			long startTime = System.currentTimeMillis();
			do {
				response = tapEnv.executeCommandUsingSsh(device,
						BroadBandCommandConstants.CMD_TO_GET_ACS_URL_TR69_LOGS);

				LOGGER.info("STEP 1: response " + response);
				LOGGER.info("STEP 1: check pattern finder"
						+ CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TO_GET_ACS_URL)
								.equalsIgnoreCase(BroadBandTestConstants.MANAGEMENT_SERVER_URL));
				status = CommonMethods.isNotNull(response)
						&& CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TO_GET_ACS_URL)
								.equalsIgnoreCase(BroadBandTestConstants.MANAGEMENT_SERVER_URL);
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWO_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TEN_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 1:ACTUAL :Validation of ACS url in TR69 log is successful");
			} else {
				LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

			/**
			 * STEP 2:Perform Get operation using tr69 .
			 */
			status = false;
			testStep = "s2";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 2 : DESCRIPTION :PERFORM GET OPERATION USING TR69 .");
			LOGGER.info("STEP 2 : ACTION :EXECUTE TR69 COMMAND WITH PARAM : Device.DeviceInfo.SerialNumber");
			LOGGER.info("STEP 2 : EXPECTED:TR69 COMMAND EXECUTION SHOULD BE SUCCESSFUL");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to execute TR69 command/Validate serial number";
			tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);
			LOGGER.info("Waiting for 4 minutes for TR69 connection to establish");
			startTime = System.currentTimeMillis();
			do {
				List<String> get_param = new ArrayList<String>();
				get_param = BroadBandTr69Utils.getParameterForTr69Get(TR69ParamConstants.TR69_PARAM_SERIAL_NUMBER);
				String tr69DeviceSerialNumber = tapEnv.getTR69ParameterValues(device, get_param);
				LOGGER.info("Result of get operation is:" + tr69DeviceSerialNumber);

				String manufacturerSerialNumber = tapEnv.executeWebPaCommand(device,
						BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_SERIAL_NUMBER);
				if (CommonMethods.isNotNull(tr69DeviceSerialNumber)
						&& CommonMethods.isNotNull(manufacturerSerialNumber)) {
					status = (tr69DeviceSerialNumber.trim()).equalsIgnoreCase(manufacturerSerialNumber.trim());

				}
			} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !status
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
			if (status) {
				LOGGER.info("STEP 2:ACTUAL :TR69 command executed successfully");
			} else {
				LOGGER.error("STEP 2:ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

			/**
			 * STEP 3:Perform Set operation using tr69
			 */
			status = false;
			testStep = "s3";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3 : DESCRIPTION :PERFORM SET OPERATION USING TR69 ");
			LOGGER.info(
					"STEP 3 : ACTION :EXECUTE TR69 COMMAND WITH PARAM Device.WiFi.Radio.10000.Channel with Random Availble value");
			LOGGER.info("STEP 3 : EXPECTED:TR69 COMMAND EXECUTION SHOULD BE SUCCESSFUL");
			LOGGER.info("**********************************************************************************");
			errorMessage = "";

			String possibleChannelList = AutomaticsPropertyUtility
					.getProperty(BroadBandTestConstants.PROP_KEY_POSSIBLE_CHANNELLIST_RADIO_ONE);

			String[] possibleChannelList2Ghz = possibleChannelList.split(",");

			try {
				Parameter setParam = new Parameter();

				setParam.setDataType(TR69ParamDataType.UNSIGNED_INT.get());
				setParam.setParamName(TR69ParamConstants.TR69_PARAM_CHANNEL_NUMBER_2GHZ);
				setParam.setParamValue(possibleChannelList2Ghz[0]);
				List<Parameter> parameters = new ArrayList<Parameter>();
				parameters.add(setParam);

				response = tapEnv.setTR69ParameterValues(device, parameters);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
			} catch (Exception exception) {
				// Log & Suppress the Exception.
				LOGGER.error(
						"EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: " + exception.getMessage());
			}

			if (status) {
				LOGGER.info("STEP 3:ACTUAL :Tr69 command executed successfully");
			} else {
				LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

			/**
			 * STEP 4:Perform webpa command to validate ACS Management URL
			 */
			status = false;
			testStep = "s4";
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 4 : DESCRIPTION :PERFORM WEBPA COMMAND TO VALIDATE ACS MANAGEMENT URL ");
			LOGGER.info("STEP 4 : ACTION :EXECUTE WEBPA GET COMMAND WITH PARAM Device.ManagementServer.URL");
			LOGGER.info("STEP 4 : EXPECTED:WEBPA SHOULD BE SUCCESSFUL AND URL SHOULD BE AVAILBLE");
			LOGGER.info("**********************************************************************************");
			errorMessage = "unable to perform webpa get command";
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_URL);
			LOGGER.info("STEP 4: response " + response);
			LOGGER.info("STEP 4: response.trim() " + response.trim());
			LOGGER.info("STEP 4: BroadBandTestConstants.MANAGEMENT_SERVER_URL "
					+ BroadBandTestConstants.MANAGEMENT_SERVER_URL);
			LOGGER.info("STEP 4: response.trim().equals(BroadBandTestConstants.MANAGEMENT_SERVER_URL)"
					+ response.trim().equals(BroadBandTestConstants.MANAGEMENT_SERVER_URL));
			if (response.trim().equals(BroadBandTestConstants.MANAGEMENT_SERVER_URL)) {
				status = true;
				LOGGER.info("STEP 4:ACTUAL :Webpa get command executed and Url validated successfully");
			} else {
				LOGGER.error("STEP 4:ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

			/**
			 * STEP 5:Perform Webpa command to validate ACS Management connection request
			 * port is not null
			 */
			status = false;
			testStep = "s5";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 5 : DESCRIPTION :PERFORM WEBPA COMMAND TO VALIDATE ACS MANAGEMENT CONNECTION REQUEST PORT IS NOT NULL");
			LOGGER.info(
					"STEP 5 : ACTION :Execute WEBPA GET COMMAND WITH PARAM Device.ManagementServer.X_CISCO_COM_ConnectionRequestURLPort");
			LOGGER.info("STEP 5 : EXPECTED:WEBPA SHOULD BE SUCCESSFUL ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to Perform WebPa get operation";
			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_CONNECTION_PORT);
			if (CommonMethods.isNotNull(response)) {
				status = true;
				LOGGER.info("STEP 5:ACTUAL :Webpa get command executed successfully");
			} else {
				LOGGER.error("STEP 5:ACTUAL :" + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception in test to verify ACS Validation" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
		}
	}

	/**
	 * 
	 * This method verifies whether WiFi objects can be enabled disabled via TR69
	 * 
	 * <ol>
	 * <li>stepNumber 1: Execute get Parameter on Device.WiFi.SSID.10001.SSID to get
	 * the current 2.4 GHZ private WiFi SSID.</li>
	 * <li>stepNumber 2: Execute set parameter on Device.WiFi.SSID.10001.SSID with a
	 * new SSID value.</li>
	 * <li>stepNumber 3: Verify the new SNMP get on the MIB
	 * .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 for new SSID.</li>
	 * <li>stepNumber 4: Verify the new SSID from WebPa using the parameter
	 * Device.WiFi.SSID.10001.SSID</li>
	 * <li>stepNumber 5: Execute get Parameter on Device.WiFi.SSID.10001.SSID and
	 * verify whether the new SSID is being returned</li>
	 * <li>stepNumber 6: Execute get Parameter on Device.WiFi.SSID.10101.SSID to get
	 * the current 5 GHZ private WiFi SSID.</li>
	 * <li>stepNumber 7: Execute set parameter on Device.WiFi.SSID.10101.SSID with a
	 * new SSID value.</li>
	 * <li>stepNumber 8: Verify the new SNMP get on the MIB
	 * .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101 for new SSID.</li>
	 * <li>stepNumber 9: Verify the new SSID from WebPa using the parameter
	 * Device.WiFi.SSID.10101.SSID</li>
	 * <li>stepNumber 10: Execute get Parameter on Device.WiFi.SSID.10101.SSID and
	 * verify whether the new SSID is being returned</li>
	 * </ol>
	 * 
	 * @author Sathurya Ravi
	 * @refactor Govardhan
	 * 
	 * @param device Dut to be used for execution
	 * 
	 */

	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
			BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
	@TestDetails(testUID = "TC-RDKB-TR69-1009")

	public void testVerifyTR69PrivateWifiSSIDNameChange(Dut device) {

		// stores the test result
		boolean status = false;
		// stores the test case id
		String testId = "TC-RDKB-TR69-009";
		// stores the error message
		String errorMessage = null;
		// stores the stepNumber
		String stepNumber = null;
		// stores the command response
		String response = null;
		// stores new SSID name
		String newSsidName = null;
		// stores old SSID name
		String oldSsidName = null;

		List<String> get_param = new ArrayList<String>();

		try {

			/*
			 * stepNumber 1: Execute get Parameter on Device.WiFi.SSID.10001.SSID to get the
			 * current 2.4 GHZ private WiFi SSID.
			 */

			LOGGER.info(
					"******************************************************************************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION: Get the current SSID value from TR69");
			LOGGER.info(
					"STEP 1: ACTION: Execute get Parameter on Device.WiFi.SSID.10001.SSID to get the current 2.4 GHZ private WiFi SSID.");
			LOGGER.info("STEP 1: EXPECTED: The parameter should return a response without any error ");
			LOGGER.info(
					"******************************************************************************************************************************************");

			stepNumber = "s1";
			status = false;

			get_param.clear();

			try {
				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
				oldSsidName = tapEnv.getTR69ParameterValues(device, get_param);
				LOGGER.info("Result of get operation is:" + oldSsidName);

			} catch (Exception e) {
				LOGGER.error("Exception occured while parsing the TR69 response", e);
			}

			status = CommonMethods.isNotNull(oldSsidName);

			errorMessage = "Not able to get value for Device.WiFi.SSID.10001.SSID via TR69 parameter. Actual: "
					+ oldSsidName;

			if (status) {
				LOGGER.info("STEP 1: ACTUAL: Parameter got a response Successfully");
			} else {
				LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 2: Execute set parameter on Device.WiFi.SSID.10001.SSID with a new
			 * SSID value.
			 */

			LOGGER.info(
					"******************************************************************************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION: Change the private 2.4 GHZ SSID name via TR69");
			LOGGER.info("STEP 2: ACTION: Exceute set parameter on Device.WiFi.SSID.10001.SSID with a new SSID value.");
			LOGGER.info("STEP 2: EXPECTED: The parameter should get set without any errors ");
			LOGGER.info(
					"******************************************************************************************************************************************");

			stepNumber = "s2";
			status = false;

			errorMessage = "Not able to set value for Device.WiFi.SSID.10001.SSID via TR69 set parameter ";

			newSsidName = BroadBandTestConstants.APPEND_STRING_FOR_24_GHZ_SSID_NAME_CHANGE
					.concat(Long.toString(System.currentTimeMillis()));

			try {
				Parameter setParam = new Parameter();
				setParam.setDataType(TR69ParamDataType.STRING.get());
				setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
				setParam.setParamValue(newSsidName);
				List<Parameter> parameters = new ArrayList<Parameter>();
				parameters.add(setParam);

				response = tapEnv.setTR69ParameterValues(device, parameters);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
			} catch (Exception exception) {
				// Log & Suppress the Exception.
				LOGGER.error(
						"EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: " + exception.getMessage());
			}

			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(RDKBTestConstants.STRING_CONNECTION_STATUS);

			if (status) {
				LOGGER.info("STEP 2: ACTUAL: Successfully Parameter got set without any errors");
			} else {
				LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 3: Execute SNMP get on the MIB
			 * .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 to get 2.4 GHZ private WiFi SSID.
			 */

			LOGGER.info(
					"******************************************************************************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION: Get the new SSID name from the SNMP get command");
			LOGGER.info(
					"STEP 3: ACTION: Execute SNMP get on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 to get 2.4 GHZ private WiFi SSID.");
			LOGGER.info(
					"STEP 3: EXPECTED: The SNMP get operation should be successful and give the new SSID in response");
			LOGGER.info(
					"******************************************************************************************************************************************");

			stepNumber = "s3";
			status = false;

			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getTableIndex());

			errorMessage = "SNMP get on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 did not return expected value. Actual: "
					+ response;

			status = CommonMethods.isNotNull(response) && response.equals(newSsidName);

			if (status) {
				LOGGER.info("STEP 3: ACTUAL: SNMP get operation is successful and gave the new SSID");
			} else {
				LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 4: Verify the new SSID from WebPa using the parameter
			 * Device.WiFi.SSID.10001.SSID
			 */

			LOGGER.info(
					"***************************************************************************************************************************************************");
			LOGGER.info("STEP 4: DESCRIPTION: Verify the SSID change from the WebPa response");
			LOGGER.info(
					"STEP 4: ACTION: Verify the new SSID from WebPa using the parameter Device.WiFi.SSID.10001.SSID .");
			LOGGER.info("STEP 4: EXPECTED: The WebPa response should return the new SSID");
			LOGGER.info(
					"***************************************************************************************************************************************************");

			stepNumber = "s4";
			status = false;

			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);

			status = CommonMethods.isNotNull(response) && response.equals(newSsidName);

			errorMessage = "The WebPa is not as expected. Actual value: " + response;

			if (status) {
				LOGGER.info("STEP 4: ACTUAL: WebPa response returned the new SSID successfully");
			} else {
				LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 5: Execute get Parameter on Device.WiFi.SSID.10001.SSID and verify
			 * whether the new SSID is being returned.
			 */

			LOGGER.info(
					"***************************************************************************************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION: Validate change of SSID name from TR69 parameters ");
			LOGGER.info(
					"STEP 5: ACTION: Execute get Parameter on Device.WiFi.SSID.10001.SSID and verify whether the new SSID is being returned.");
			LOGGER.info("STEP 5: EXPECTED: The get Parameter should return new SSID name");
			LOGGER.info(
					"***************************************************************************************************************************************************");

			stepNumber = "s5";
			status = false;

			errorMessage = "Old and new SSIDs are same. Therefore , SSID name change was not successful ";

			get_param.clear();

			try {
				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
				response = tapEnv.getTR69ParameterValues(device, get_param);
				LOGGER.info("Result of get operation is:" + response);

			} catch (Exception e) {
				LOGGER.error("Exception occured while parsing the TR69 response", e);
			}

			status = CommonMethods.isNotNull(response) && response.equals(newSsidName);

			if (status) {
				LOGGER.info("STEP 5: ACTUAL: Parameter returned new SSID name successfully");
			} else {
				LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 6: Execute get Parameter on Device.WiFi.SSID.10101.SSID to get the
			 * current 5 GHZ private WiFi SSID.
			 */

			LOGGER.info(
					"******************************************************************************************************************************************");
			LOGGER.info("STEP 6: DESCRIPTION: Get the current 5GHZ SSID value from TR69");
			LOGGER.info(
					"STEP 6: ACTION: Execute get Parameter on Device.WiFi.SSID.10101.SSID to get the current 5 GHZ private WiFi SSID.");
			LOGGER.info("STEP 6: EXPECTED: The parameter should return a response without any error ");
			LOGGER.info(
					"******************************************************************************************************************************************");

			stepNumber = "s6";
			status = false;

			errorMessage = "Not able to get value for Device.WiFi.SSID.10101.SSID via TR69 parameter ";

			get_param.clear();

			try {
				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
				oldSsidName = tapEnv.getTR69ParameterValues(device, get_param);
				LOGGER.info("Result of get operation is:" + oldSsidName);

			} catch (Exception e) {
				LOGGER.error("Exception occured while parsing the TR69 response", e);
			}

			status = CommonMethods.isNotNull(oldSsidName);

			if (status) {
				LOGGER.info("STEP 6: ACTUAL: Parameter returned a response without any error");
			} else {
				LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 7: Execute set parameter on Device.WiFi.SSID.10101.SSID with a new
			 * SSID value.
			 */

			LOGGER.info(
					"******************************************************************************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION: Change the private 5 GHZ SSID name via TR69");
			LOGGER.info("STEP 7: ACTION: Exceute set parameter on Device.WiFi.SSID.10101.SSID with a new SSID value.");
			LOGGER.info("STEP 7: EXPECTED: The parameter should get set without any errors ");
			LOGGER.info(
					"******************************************************************************************************************************************");

			stepNumber = "s7";
			status = false;

			errorMessage = "Not able to set value for Device.WiFi.SSID.10101.SSID via TR69 set parameter ";

			newSsidName = BroadBandTestConstants.APPEND_STRING_FOR_5_GHZ_SSID_NAME_CHANGE
					.concat(Long.toString(System.currentTimeMillis()));

			try {
				Parameter setParam = new Parameter();
				setParam.setDataType(TR69ParamDataType.STRING.get());
				setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
				setParam.setParamValue(newSsidName);
				List<Parameter> parameters = new ArrayList<Parameter>();
				parameters.add(setParam);

				response = tapEnv.setTR69ParameterValues(device, parameters);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
			} catch (Exception exception) {
				// Log & Suppress the Exception.
				LOGGER.error(
						"EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: " + exception.getMessage());
			}

			status = CommonMethods.isNotNull(response)
					&& response.equalsIgnoreCase(RDKBTestConstants.STRING_CONNECTION_STATUS);

			if (status) {
				LOGGER.info("STEP 7: ACTUAL: Parameter get set successfully without any errors");
			} else {
				LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 8: Execute SNMP get on the MIB
			 * .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101 to get 5 GHZ private WiFi SSID.
			 */

			LOGGER.info(
					"******************************************************************************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION: Get the new SSID name from the SNMP get command");
			LOGGER.info(
					"STEP 8: ACTION: Execute SNMP get on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101 to get 5 GHZ private WiFi SSID.");
			LOGGER.info(
					"STEP 8: EXPECTED: The SNMP get operation should be successful and give the new SSID in response");
			LOGGER.info(
					"******************************************************************************************************************************************");

			stepNumber = "s8";
			status = false;

			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getOid(),
					BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getTableIndex());

			errorMessage = "SNMP get on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101 did not return expected value. Actual: "
					+ response;

			status = CommonMethods.isNotNull(response) && response.equals(newSsidName);

			if (status) {
				LOGGER.info("STEP 8: ACTUAL: SNMP get operation is successful and gave the new SSID in response");
			} else {
				LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
			}

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 9: Verify the new SSID from WebPa using the parameter
			 * Device.WiFi.SSID.10101.SSID
			 */

			LOGGER.info(
					"***************************************************************************************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION: Verify the SSID change from the WebPa response");
			LOGGER.info(
					"STEP 9: ACTION: Verify the new SSID from WebPa using the parameter Device.WiFi.SSID.10101.SSID .");
			LOGGER.info("STEP 9: EXPECTED: The WebPa response should return the new SSID");
			LOGGER.info(
					"***************************************************************************************************************************************************");

			stepNumber = "s9";
			status = false;

			response = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);

			status = CommonMethods.isNotNull(response) && response.equals(newSsidName);

			errorMessage = "The WebPa is not as expected. Actual value: " + response;

			if (status) {
				LOGGER.info("STEP 9: ACTUAL:  WebPa response returned the new SSID Successfully");
			} else {
				LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 10: Execute get Parameter on Device.WiFi.SSID.10101.SSID and
			 * verify whether the new SSID is being returned.
			 */

			LOGGER.info(
					"***************************************************************************************************************************************************");
			LOGGER.info("STEP 10: DESCRIPTION: Validate change of SSID name from TR69 parameters ");
			LOGGER.info(
					"STEP 10: ACTION: Execute get Parameter on Device.WiFi.SSID.10101.SSID and verify whether the new SSID is being returned.");
			LOGGER.info("STEP 10: EXPECTED: The get Parameter should return new SSID name");
			LOGGER.info(
					"***************************************************************************************************************************************************");

			stepNumber = "s10";
			status = false;

			errorMessage = "Old and new SSIDs are same. Therefore , SSID name change was not successful ";

			get_param.clear();
			try {
				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
				response = tapEnv.getTR69ParameterValues(device, get_param);
				LOGGER.info("Result of get operation is:" + response);

			} catch (Exception e) {
				LOGGER.error("Exception occured while parsing the TR69 response", e);
			}

			status = CommonMethods.isNotNull(response) && response.equals(newSsidName);

			if (status) {
				LOGGER.info("STEP 10: ACTUAL: Parameter returned new SSID name successfully");
			} else {
				LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
			}
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

		} catch (Exception exception) {

			errorMessage = exception.getMessage();
			LOGGER.error("Exception occured while validating WiFi SSID name change from TR69 " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, errorMessage, true);

		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-TR69-1009");
	}

	/**
	 * This method verifies whether WiFi objects can be enabled disabled via TR69
	 * 
	 * <ol>
	 * <li>stepNumber 1: Execute set Parameter on Device.WiFi.Radio.10000.Enable(2.4
	 * GHZ) to False</li>
	 * <li>stepNumber 2: Verify from WebPa that 2.4 GHZ radio is disabled</li>
	 * <li>stepNumber 3: Verify from SNMP that 2.4 GHZ radio is disabled</li>
	 * <li>stepNumber 4: Execute get Parameter on the Device.WiFi.Radio.10000.Enable
	 * and verify that it is false
	 * <li>stepNumber 5: Execute get Parameter on Device.WiFi.Radio.10000.Status and
	 * verify that it is down
	 * <li>stepNumber 6: Execute get Parameter on Device.WiFi.Radio.10000.LastChange
	 * and verify that it shows appropriate time in seconds</li>
	 * <li>stepNumber 7: Execute set Parameter on Device.WiFi.Radio.10000.Enable
	 * (2.4 GHZ) to True</li>
	 * <li>stepNumber 8: Verify from WebPa that 2.4 GHZ radio is enabled</li>
	 * <li>stepNumber 9: Verify from SNMP that 2.4 GHZ radio is enabled</li>
	 * <li>stepNumber 10: Execute get Parameter on the
	 * Device.WiFi.Radio.10000.Enable and verify that it is true
	 * <li>stepNumber 11: Execute get Parameter on Device.WiFi.Radio.10000.Status
	 * and verify that it is Up
	 * <li>stepNumber 12: Execute get Parameter on
	 * Device.WiFi.Radio.10000.LastChange and verify that it shows appropriate time
	 * in seconds</li>
	 * <li>stepNumber 13: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5
	 * GHZ) to False</li>
	 * <li>stepNumber 14: Verify from WebPa that 5 GHZ radio is disabled</li>
	 * <li>stepNumber 15: Verify from SNMP that 2.4 GHZ radio is disabled</li>
	 * <li>stepNumber 16: Execute get Parameter on the
	 * Device.WiFi.Radio.10100.Enable and verify that it is false
	 * <li>stepNumber 17: Execute get Parameter on Device.WiFi.Radio.10100.Status
	 * and verify that it is down
	 * <li>stepNumber 18: Execute get Parameter on
	 * Device.WiFi.Radio.10100.LastChange and verify that it shows appropriate time
	 * in seconds</li>
	 * <li>stepNumber 19: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5
	 * GHZ) to True</li>
	 * <li>stepNumber 20: Verify from WebPa that 5 GHZ radio is enabled</li>
	 * <li>stepNumber 21: Verify from SNMP that 5 GHZ radio is enabled</li>
	 * <li>stepNumber 22: Execute get Parameter on the
	 * Device.WiFi.Radio.10100.Enable and verify that it is true
	 * <li>stepNumber 23: Execute get Parameter on Device.WiFi.Radio.10100.Status
	 * and verify that it is Up
	 * <li>stepNumber 24: Execute get Parameter on
	 * Device.WiFi.Radio.10100.LastChange and verify that it shows appropriate time
	 * in seconds</li>
	 * </ol>
	 * 
	 * @author Sathurya Ravi
	 * @refactor Rakesh C N, Sruthi Santhosh
	 * @param device device to be used for execution
	 */
	@Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
	@TestDetails(testUID = "TC-RDKB-TR69-1007")
	public void testVerifyTR69WiFiRadioEnableDisable(Dut device) {

		// stores the test result
		boolean status = false;
		// stores the test case id
		String testId = "TC-RDKB-TR69-007";
		// stores the error message
		String errorMessage = null;
		// stores the stepNumber
		String stepNumber = null;
		// stores the command response
		String response = null;
		// stores last change in seconds for radio
		int lastChange = 0;
		// stores start time in milliseconds
		long startTimeStamp = 0;
		List<Parameter> set_param = new ArrayList<Parameter>();
		List<String> get_param = new ArrayList<String>();

		try {

			/*
			 * stepNumber 1: Execute set Parameter on Device.WiFi.Radio.10000.Enable (2.4
			 * GHZ) to False
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info("stepNumber 1: DESCRIPTION: Disable 2.4 GHZ WiFi radio via TR69 ");
			LOGGER.info(
					"stepNumber 1: ACTION: set TR69 parameter Device.WiFi.Radio.10000.Enable to false to disable 2.4 GHZ radio ");
			LOGGER.info("stepNumber 1: EXPECTED: The parameter should get set without any error ");
			LOGGER.info("**********************************************************************************");

			stepNumber = "s1";
			status = false;
			set_param.clear();
			// ************************************************************************************************
			try {
				Parameter setParam = new Parameter();

				setParam.setDataType(TR69ParamDataType.BOOLEAN.get());
				setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);
				setParam.setParamValue(AutomaticsConstants.FALSE);
				List<Parameter> parameters = new ArrayList<Parameter>();
				parameters.add(setParam);

				response = tapEnv.setTR69ParameterValues(device, parameters);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
			} catch (Exception exception) {
				// Log & Suppress the Exception.
				LOGGER.error(
						"EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: " + exception.getMessage());
			}

			// ***************************************************************************************************

			errorMessage = "Not able to set false for 2.4 GHz radio using Device.WiFi.Radio.10000.Enable tr69 params ";

			if (status)
				LOGGER.info("STEP 1 ACTUAL : WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE set to false");
			else
				LOGGER.error("STEP 1 ACTUAL : " + errorMessage);

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			// Introducing wait time to make sure the operation is completed
			// before further validation

			Thread.sleep(100000);

			/*
			 * stepNumber 2: Verify from WebPa the 2.4GHZ radio are disabled
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info("stepNumber 2: DESCRIPTION: Validate the radio status from WEBpa ");
			LOGGER.info("stepNumber 2: ACTION: Verify from WebPa using Device.WiFi.Radio.10000.Enable ");
			LOGGER.info("stepNumber 2: EXPECTED: The response should be false ");
			LOGGER.info("**********************************************************************************");

			stepNumber = "s2";
			status = false;

			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);

			if (response != null)
				status = response.equalsIgnoreCase(AutomaticsConstants.FALSE);

			errorMessage = "The WebPa parameter Device.WiFi.Radio.10000.Enable is not returning appropriate value. Actual value: "
					.concat(response);

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 3: Verify using SNMP the 2.4 GHZ radio is disabled using
			 * 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10000
			 */

			LOGGER.info(
					"******************************************************************************************************************************");
			LOGGER.info("stepNumber 3: DESCRIPTION: validate via SNMP that 2.4 GHZ radio is disabled ");
			LOGGER.info(
					"stepNumber 3: ACTION: Verify using SNMP the private 2.4GHZ SSID is disabled using  1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10000 ");
			LOGGER.info("stepNumber 3: EXPECTED: The MIB should return an integer 3");
			LOGGER.info(
					"******************************************************************************************************************************");

			stepNumber = "s3";
			status = false;

			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_2_4_GHZ.getOid(),
					BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_2_4_GHZ.getTableIndex());

			status = CommonMethods.isNotNull(response)
					&& (Integer.parseInt(response) == AutomaticsConstants.CONSTANT_1);

			errorMessage = "The MIB 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10000 has not returned the expected value. Actual: "
					+ response;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 4: Execute get Parameter on the Device.WiFi.Radio.10000.Enable and
			 * verify that it is false
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 4: DESCRIPTION: Execute get Parameter on the Device.WiFi.Radio.10000.Enable and verify that it is false ");
			LOGGER.info("stepNumber 4: ACTION: Verify from the Device.WiFi.Radio.10000.Enable that 2.4 GHZ is false ");
			LOGGER.info("stepNumber 4: EXPECTED: The parameters should return the false");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s4";
			status = false;
			String result = "";

			get_param.clear();

			try {

				get_param = BroadBandTr69Utils
						.getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);
				result = tapEnv.getTR69ParameterValues(device, get_param);
				LOGGER.info("Result of get operation is:" + result);
				status = result.equalsIgnoreCase(AutomaticsConstants.FALSE);

			} catch (Exception e) {
				LOGGER.error("Exception occured while parsing the TR69 response", e);
			}

			errorMessage = "The parameter Device.WiFi.Radio.10000.Enable did not return the expected value. Actual value: "
					+ status;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 5: Execute get Parameter on Device.WiFi.Radio.10000.Status and
			 * verify that it is down
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 5: DESCRIPTION: Execute get Parameter on Device.WiFi.Radio.10000.Status and verify that it is down ");
			LOGGER.info("stepNumber 5: ACTION: Verify from the Device.WiFi.Radio.10000.Status that 2.4 GHZ is down ");
			LOGGER.info("stepNumber 5: EXPECTED: The parameters should return the value down");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s5";
			status = false;
			result = "";
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_2_4GHZ);
				result = tapEnv.getTR69ParameterValues(device, get_param);
				LOGGER.info("Result of get operation is:" + result);
				status = result.equalsIgnoreCase(BroadBandTestConstants.STRING_DOWN);

			} catch (Exception exception) {
				LOGGER.error(exception.getMessage());
			}

			errorMessage = "The parameter Device.WiFi.Radio.10000.Status did not return the expected value. Actual value: "
					+ status;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 6: Execute get Parameter on Device.WiFi.Radio.10000.LastChange and
			 * verify that it shows appropriate time in seconds
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 6: DESCRIPTION: Execute get Parameter on Device.WiFi.Radio.10000.LastChange and verify that it shows appropriate time in seconds ");
			LOGGER.info(
					"stepNumber 6: ACTION: Verify from the Device.WiFi.Radio.10000.LastChange that last change value is valid");
			LOGGER.info("stepNumber 6: EXPECTED: The parameters should valid time in seconds");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s6";
			status = false;
			result = "";
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_LAST_CHANGE_FOR_2_4GHZ);
				lastChange = Integer.parseInt(tapEnv.getTR69ParameterValues(device, get_param));

				status = lastChange < ((System.currentTimeMillis() - startTimeStamp) + 10000);

			} catch (NumberFormatException exception) {
				response = exception.getMessage();
			}

			errorMessage = "The parameter Device.WiFi.Radio.10000.LastChange did not return the expected value. Actual value: "
					+ (System.currentTimeMillis() - startTimeStamp) + 10000;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 7: Execute set Parameter on Device.WiFi.Radio.10000.Enable (2.4
			 * GHZ) to True
			 * 
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info("stepNumber 7: DECSRIPTION: Execute set parameter on to enable the radio again ");
			LOGGER.info(
					"stepNumber 7: ACTION: Execute set Parameter on Device.WiFi.Radio.10000.Enable (2.4 GHZ) to True ");
			LOGGER.info("stepNumber 7: EXPECTED: The parameter should get set without any errors ");
			LOGGER.info(
					"***************************************************************************************************************");

			stepNumber = "s7";
			status = false;
			result = "";
			set_param.clear();
			// ************************************************************************************************
			try {
				Parameter setParam = new Parameter();

				setParam.setDataType(TR69ParamDataType.BOOLEAN.get());
				setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);
				setParam.setParamValue(AutomaticsConstants.TRUE);
				List<Parameter> parameters = new ArrayList<Parameter>();
				parameters.add(setParam);

				response = tapEnv.setTR69ParameterValues(device, parameters);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
			} catch (Exception exception) {
				// Log & Suppress the Exception.
				LOGGER.error(
						"EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: " + exception.getMessage());
			}

			// ***************************************************************************************************
			errorMessage = "Not able to set true for 2.4 GHz radio using Device.WiFi.Radio.10000.Enable tr69 params ";

			if (status)
				LOGGER.info("STEP 7 ACTUAL : WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE set to true");
			else
				LOGGER.error("STEP 7 ACTUAL : " + errorMessage);

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			// Introducing wait time to make sure the operation is completed
			// before further validation

			Thread.sleep(100000);

			/*
			 * stepNumber 8: Verify from WebPa that 2.4 GHZ radio is enabled
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info("stepNumber 8: DESCRIPTION: Verify from WebPa that TR69 is enabled ");
			LOGGER.info(
					"stepNumber 8: ACTION: execute WebPa command on the parameter Device.WiFi.Radio.10000.Enable to check the enable status ");
			LOGGER.info("stepNumber 8: EXPECTED: The WebPa command should execute appropriate response ");
			LOGGER.info(
					"***************************************************************************************************************");

			stepNumber = "s8";
			status = false;

			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);

			if (null != response)
				status = response.equalsIgnoreCase(AutomaticsConstants.TRUE);

			errorMessage = "The Webpa parameter Device.WiFi.Radio.10000.Enable has not returned expected response. Actual: "
					.concat(response);

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 9: Verify using SNMP the private 2.4GHZ SSID is disabled using
			 * 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10000
			 */

			LOGGER.info(
					"******************************************************************************************************************************");
			LOGGER.info("stepNumber 9: DESCRIPTION: validate via SNMP that 2.4 GHZ radio is enabled ");
			LOGGER.info(
					"stepNumber 9: ACTION: Verify using SNMP the private 2.4GHZ SSID is enabled using  1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10000 ");
			LOGGER.info("stepNumber 9: EXPECTED: The MIB should return an integer 3");
			LOGGER.info(
					"******************************************************************************************************************************");

			stepNumber = "s9";
			status = false;

			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_2_4_GHZ.getOid(),
					BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_2_4_GHZ.getTableIndex());

			status = CommonMethods.isNotNull(response)
					&& (Integer.parseInt(response) == AutomaticsConstants.CONSTANT_3);

			errorMessage = "The MIB 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10000 has not returned the expected value. Actual: "
					+ response;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 10: Execute get Parameter on the Device.WiFi.Radio.10000.Enable
			 * and verify that it is true
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 10: DESCRIPTION: Execute get Parameter on the Device.WiFi.Radio.10000.Enable and verify that it is true ");
			LOGGER.info("stepNumber 10: ACTION: Verify from the Device.WiFi.Radio.10000.Enable that 2.4 GHZ is true ");
			LOGGER.info("stepNumber 10: EXPECTED: The parameters should return the true");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s10";
			status = false;
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils
						.getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE);

				status = tapEnv.getTR69ParameterValues(device, get_param).equalsIgnoreCase(AutomaticsConstants.TRUE);

			} catch (Exception e) {
				LOGGER.error("Exception occured while parsing the TR69 response", e);
			}

			errorMessage = "The parameter Device.WiFi.Radio.10000.Enable did not return the expected value. Actual value: "
					+ status;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 11: Execute get Parameter on Device.WiFi.Radio.10000.Status and
			 * verify that it is up
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 11: DESCRIPTION: Execute get Parameter on Device.WiFi.Radio.10000.Status and verify that it is up ");
			LOGGER.info("stepNumber 11: ACTION: Verify from the Device.WiFi.Radio.10000.Status that 2.4 GHZ is up ");
			LOGGER.info("stepNumber 11: EXPECTED: The parameters should return the value up");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s11";
			status = false;
			get_param.clear();

			try {
				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_2_4GHZ);

				status = tapEnv.getTR69ParameterValues(device, get_param)
						.equalsIgnoreCase(BroadBandTestConstants.STRING_UP);

			} catch (Exception exception) {
				response = exception.getMessage();
			}

			errorMessage = "The parameter  Device.WiFi.Radio.10000.Status did not return the expected value. Actual value: "
					+ status;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 12: Execute get Parameter on Device.WiFi.Radio.10000.LastChange
			 * and verify that it shows appropriate time in seconds
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 12: DESCRIPTION: Execute get Parameter on Device.WiFi.Radio.10000.LastChange and verify that it shows appropriate time in seconds ");
			LOGGER.info(
					"stepNumber 12: ACTION: Verify from the Device.WiFi.Radio.10000.LastChange that last change value is valid");
			LOGGER.info("stepNumber 12: EXPECTED: The parameters should valid time in seconds");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s12";
			status = false;
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_LAST_CHANGE_FOR_2_4GHZ);

				lastChange = Integer.parseInt(tapEnv.getTR69ParameterValues(device, get_param));

				// Adding 10 seconds extra to (lastChange <
				// (System.currentTimeMillis() - startTimeStamp) to get the
				// approximate seconds value
				status = lastChange < (System.currentTimeMillis() - startTimeStamp) + 10000;

			} catch (NumberFormatException exception) {
				LOGGER.error(exception.getMessage());
			}

			errorMessage = "The parameter Device.WiFi.Radio.10000.LastChange did not return the expected value. Actual value: "
					+ (System.currentTimeMillis() - startTimeStamp) + 10000;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 13: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5
			 * GHZ) to False
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info("stepNumber 13: DESCRIPTION: Disable 5 GHZ WiFi radio via TR69 ");
			LOGGER.info(
					"stepNumber 13: ACTION: stepNumber 7: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5 GHZ) to False ");
			LOGGER.info("stepNumber 13: EXPECTED: The parameter should get set without any error ");
			LOGGER.info("**********************************************************************************");

			stepNumber = "s13";
			status = false;
			set_param.clear();

			// ************************************************************************************************
			try {
				Parameter setParam = new Parameter();

				setParam.setDataType(TR69ParamDataType.BOOLEAN.get());
				setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);
				setParam.setParamValue(AutomaticsConstants.FALSE);
				List<Parameter> parameters = new ArrayList<Parameter>();
				parameters.add(setParam);

				response = tapEnv.setTR69ParameterValues(device, parameters);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
			} catch (Exception exception) {
				// Log & Suppress the Exception.
				LOGGER.error(
						"EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: " + exception.getMessage());
			}

			// ***************************************************************************************************

			errorMessage = "Not able to set false for 5 GHz radio using Device.WiFi.Radio.10100.Enable tr69 params ";

			if (status)
				LOGGER.info("STEP 13 ACTUAL : WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE set to true");
			else
				LOGGER.error("STEP 13 ACTUAL : " + errorMessage);

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			// Introducing wait time to make sure the operation is completed
			// before further validation

			Thread.sleep(100000);

			/*
			 * stepNumber 14: Verify from WebPa that 5 GHZ radio is disabled
			 */
			LOGGER.info("**********************************************************************************");
			LOGGER.info("stepNumber 14: DESCRIPTION: Validate the radio status from WEBpa ");
			LOGGER.info("stepNumber 14: ACTION: Verify from WebPa using Device.WiFi.Radio.10100.Enable ");
			LOGGER.info("stepNumber 14: EXPECTED: The response should be false ");
			LOGGER.info("**********************************************************************************");

			stepNumber = "s14";
			status = false;

			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);

			if (null != response)
				status = response.equalsIgnoreCase(AutomaticsConstants.FALSE);

			errorMessage = "The Webpa parameter Device.WiFi.Radio.10100.Enable has not returned expected value. Actual: "
					.concat(response);

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 15: Verify using SNMP the private 5GHZ SSID is disabled using
			 * 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10100
			 */

			LOGGER.info(
					"******************************************************************************************************************************");
			LOGGER.info("stepNumber 15: DESCRIPTION: validate via SNMP that 5 GHZ radio is disabled ");
			LOGGER.info(
					"stepNumber 15: ACTION: Verify using SNMP the private 5 GHZ SSID is disabled using  1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10100 ");
			LOGGER.info("stepNumber 15: EXPECTED: The MIB should return an integer 3");
			LOGGER.info(
					"******************************************************************************************************************************");

			stepNumber = "s15";
			status = false;

			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_5_GHZ.getOid(),
					BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_5_GHZ.getTableIndex());
			try {
				status = CommonMethods.isNotNull(response)
						&& (Integer.parseInt(response) == AutomaticsConstants.CONSTANT_1);
			} catch (NumberFormatException nfe) {
				LOGGER.error("Exception occured while parsing the TR69 response", nfe);
			}

			errorMessage = "The MIB 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10100 has not returned the expected value. Actual: "
					+ response;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 16: Execute get Parameter on the Device.WiFi.Radio.10100.Enable
			 * and verify that it is false
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 16: DESCRIPTION: Execute get Parameter on the Device.WiFi.Radio.10100.Enable and verify that it is false ");
			LOGGER.info("stepNumber 16: ACTION: Verify from the Device.WiFi.Radio.10100.Enable that 5 GHZ is false ");
			LOGGER.info("stepNumber 16: EXPECTED: The parameters should return the false");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s16";
			status = false;
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils
						.getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);

				status = tapEnv.getTR69ParameterValues(device, get_param).equalsIgnoreCase(AutomaticsConstants.FALSE);

			} catch (Exception exception) {
				LOGGER.error(exception.getMessage());
			}

			errorMessage = "The parameter Device.WiFi.Radio.10100.Enable did not return the expected value. Actual value: "
					+ response;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 17: Execute get Parameter on Device.WiFi.Radio.10100.Status and
			 * verify that it is down
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 17: DESCRIPTION: Execute get Parameter on Device.WiFi.Radio.10100.Status and verify that it is down ");
			LOGGER.info("stepNumber 17: ACTION: Verify from the Device.WiFi.Radio.10100.Status that 5 GHZ is down ");
			LOGGER.info("stepNumber 17: EXPECTED: The parameters should return the value down");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s17";
			status = false;
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils
						.getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_5GHZ);

				status = tapEnv.getTR69ParameterValues(device, get_param)
						.equalsIgnoreCase(BroadBandTestConstants.STRING_DOWN);

			} catch (Exception exception) {
				LOGGER.error(response = exception.getMessage());
			}

			errorMessage = "The parameter Device.WiFi.Radio.10100.Status did not return the expected value. Actual value: "
					+ status;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 18: Execute get Parameter on Device.WiFi.Radio.10100.LastChange
			 * and verify that it shows appropriate time in seconds
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 18: DESCRIPTION: Execute get Parameter on Device.WiFi.Radio.10100.LastChange and verify that it shows appropriate time in seconds ");
			LOGGER.info(
					"stepNumber 18: ACTION: Verify from the Device.WiFi.Radio.10100.LastChange that last change value is valid");
			LOGGER.info("stepNumber 18: EXPECTED: The parameters should valid time in seconds");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s18";
			status = false;
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_LAST_CHANGE_STATUS_FOR_5GHZ);

				lastChange = Integer.parseInt(tapEnv.getTR69ParameterValues(device, get_param));

				// Adding 10 seconds extra to (lastChange <
				// (System.currentTimeMillis() - startTimeStamp) to get the
				// approximate seconds value
				status = lastChange < ((System.currentTimeMillis() - startTimeStamp) + 10000);

			} catch (NumberFormatException exception) {
				LOGGER.error(response = exception.getMessage());
			}

			errorMessage = "The parameter Device.WiFi.Radio.10100.LastChange did not return the expected value. Actual value: "
					+ (System.currentTimeMillis() - startTimeStamp) + 10000;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 19: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5
			 * GHZ) to True
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info("stepNumber 19: DECSRIPTION: Execute set parameter on to enable the radio again ");
			LOGGER.info(
					"stepNumber 19: ACTION: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5 GHZ) to True ");
			LOGGER.info("stepNumber 19: EXPECTED: The parameter should get set without any errors ");
			LOGGER.info(
					"***************************************************************************************************************");

			stepNumber = "s19";
			status = false;
			set_param.clear();

			// ************************************************************************************************
			try {
				Parameter setParam = new Parameter();

				setParam.setDataType(TR69ParamDataType.BOOLEAN.get());
				setParam.setParamName(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);
				setParam.setParamValue(AutomaticsConstants.TRUE);
				List<Parameter> parameters = new ArrayList<Parameter>();
				parameters.add(setParam);

				response = tapEnv.setTR69ParameterValues(device, parameters);
				status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(AutomaticsConstants.SUCCESS);
			} catch (Exception exception) {
				// Log & Suppress the Exception.
				LOGGER.error(
						"EXCEPTION OCCURRED WHILE UPDATING THE VALUE FOR TR-69 PARAMETER: " + exception.getMessage());
			}

			errorMessage = "Not able to set true for 5 GHz radio using Device.WiFi.Radio.10100.Enable tr69 params. Actual: "
					+ response;

			if (status)
				LOGGER.info("STEP 19 ACTUAL : WEBPA_PARAM_WIFI_2_4_RADIO_ENABLE set to true");
			else
				LOGGER.error("STEP 19 ACTUAL : " + errorMessage);
			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			// Introducing wait time to make sure the operation is completed
			// before further validation

			Thread.sleep(100000);

			/*
			 * stepNumber 20: Verify from WebPa that 5 GHZ radio is enabled
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info("stepNumber 20: DESCRIPTION: Verify from WebPa that TR69 is enabled ");
			LOGGER.info(
					"stepNumber 20: ACTION: execute WebPa command on the parameter Device.WiFi.Radio.10100.Enable to check the enable status ");
			LOGGER.info("stepNumber 20: EXPECTED: The WebPa command should execute appropriate response ");
			LOGGER.info(
					"***************************************************************************************************************");

			stepNumber = "s20";
			status = false;

			response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);

			if (null != response)
				status = response.equalsIgnoreCase(AutomaticsConstants.TRUE);

			errorMessage = "The Webpa parameter Device.WiFi.Radio.10100.Enable has not returned the expected value. Actual: "
					.concat(response);

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 21: Verify using SNMP the private 5 GHZ SSID is enabled using
			 * 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10100
			 */

			LOGGER.info(
					"******************************************************************************************************************************");
			LOGGER.info("stepNumber 21: DESCRIPTION: validate via SNMP that 5 GHZ radio is enabled ");
			LOGGER.info(
					"stepNumber 21: ACTION: Verify using SNMP the private 5 GHZ SSID is enabled using  1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10100 ");
			LOGGER.info("stepNumber 21: EXPECTED: The MIB should return an integer 1");
			LOGGER.info(
					"******************************************************************************************************************************");

			stepNumber = "s21";
			status = false;

			response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
					BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_5_GHZ.getOid(),
					BroadBandSnmpMib.ENABLE_DISABLE_WIFI_RADIO_5_GHZ.getTableIndex());

			status = CommonMethods.isNotNull(response)
					&& (Integer.parseInt(response) == AutomaticsConstants.CONSTANT_3);

			errorMessage = "The MIB 1.3.6.1.4.1.17270.50.2.2.6.1.1.1.10100 has not returned the expected value. Actual: "
					+ response;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 22: Execute get Parameter on the Device.WiFi.Radio.10100.Enable
			 * and verify that it is true
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 22: DESCRIPTION: Execute get Parameter on the Device.WiFi.Radio.10100.Enable and verify that it is true ");
			LOGGER.info("stepNumber 22: ACTION: Verify from the Device.WiFi.Radio.10100.Enable that 5 GHZ is true ");
			LOGGER.info("stepNumber 22: EXPECTED: The parameters should return the true");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s22";
			status = false;
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils
						.getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_WIFI_5_RADIO_ENABLE);

				status = tapEnv.getTR69ParameterValues(device, get_param).equalsIgnoreCase(AutomaticsConstants.TRUE);

			} catch (Exception exception) {
				LOGGER.error(response = exception.getMessage());
			}

			errorMessage = "The parameter Device.WiFi.Radio.10100.Enable did not return the expected value. Actual value: "
					+ response;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 23: Execute get Parameter on Device.WiFi.Radio.10100.Status and
			 * verify that it is up
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 23: DESCRIPTION: Execute get Parameter on Device.WiFi.Radio.10100.Status and verify that it is up ");
			LOGGER.info("stepNumber 23: ACTION: Verify from the Device.WiFi.Radio.10100.Status that 5 GHZ is up ");
			LOGGER.info("stepNumber 23: EXPECTED: The parameters should return the value up");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s23";
			status = false;
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils
						.getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS_FOR_5GHZ);

				status = tapEnv.getTR69ParameterValues(device, get_param)
						.equalsIgnoreCase(BroadBandTestConstants.STRING_UP);

			} catch (Exception exception) {
				LOGGER.error(response = exception.getMessage());
			}

			errorMessage = "The parameter Device.WiFi.Radio.10100.Status did not return the expected value. Actual value: "
					+ response;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

			/*
			 * stepNumber 24: Execute get Parameter on Device.WiFi.Radio.10100.LastChange
			 * and verify that it shows appropriate time in seconds
			 */

			LOGGER.info(
					"***************************************************************************************************************");
			LOGGER.info(
					"stepNumber 24: DESCRIPTION: Execute get Parameter on Device.WiFi.Radio.10100.LastChange and verify that it shows appropriate time in seconds ");
			LOGGER.info(
					"stepNumber 24: ACTION: Verify from the Device.WiFi.Radio.10100.LastChange that last change value is valid");
			LOGGER.info("stepNumber 24: EXPECTED: The parameters should valid time in seconds");
			LOGGER.info(
					"****************************************************************************************************************");

			stepNumber = "s24";
			status = false;
			get_param.clear();

			try {

				get_param = BroadBandTr69Utils.getParameterForTr69Get(
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_LAST_CHANGE_STATUS_FOR_5GHZ);

				lastChange = Integer.parseInt(tapEnv.getTR69ParameterValues(device, get_param));

				// Adding 10 seconds extra to (lastChange <
				// (System.currentTimeMillis() - startTimeStamp) to get the
				// approximate seconds value
				status = lastChange < ((System.currentTimeMillis() - startTimeStamp) + 10000);

			} catch (NumberFormatException exception) {
				LOGGER.error(response = exception.getMessage());
			}

			errorMessage = "The parameter Device.WiFi.Radio.10100.LastChange did not return the expected value. Actual value: "
					+ (System.currentTimeMillis() - startTimeStamp) + 10000;

			tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

		} catch (Exception exception) {
			errorMessage = exception.getMessage();
			LOGGER.error("Exception occured while validating WiFi radio Enable/Disable from TR69 " + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, errorMessage, true);
		}

	}
}