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
import com.automatics.enums.StbProcess;
import com.automatics.enums.TR69ParamDataType;
import com.automatics.exceptions.TestException;
import com.automatics.providers.tr69.Parameter;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.TR69ParamConstants;
import com.automatics.rdkb.reboot.BootTimeUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.tr69.BroadBandTr69Utils;
import com.automatics.rdkb.utils.tr69.TR69DeviceInterfaceObjects;

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
     * Test to Verify the Manufacturer serial number using TR69 parameter - Device.DeviceInfo.SerialNumber
     * 
     * @param device
     *            The device to be used.
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
	 * Step 1 : Retrieve the Serial number using TR-69 parameter Device.DeviceInfo.SerialNumber and verify with the
	 * actual serial number of the device using device object
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
     * Test to Verify the device model name using TR69 parameter - Device.DeviceInfo.ModelName
     * 
     * @param device
     *            The device to be used.
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
	 * Step 1 : Retrieve the Device Model Name using TR-69 parameter Device.DeviceInfo.ModelName and verify with the
	 * actual model name of the device using device object
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
     * Test to Verify the current firmware version using TR69 parameter - Device.DeviceInfo.SoftwareVersion
     * 
     * @param device
     *            The device to be used.
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
	 * Step 1 : Retrieve the Currently running firmware version using TR-69 parameter
	 * Device.DeviceInfo.SoftwareVersion and verify with the actual currently running firmware version of the device
	 * using device object
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
     * <li>stepNumber 1: Execute get Parameter on Device.ManagementServer. to get the parameter values for Management of
     * the device and validate the parameters</li>
     * <li>stepNumber 2: Validate the Management Server URL</li>
     * <li>stepNumber 3: Validate the Management Server Username</li>
     * <li>stepNumber 4: Validate the Management Server Connection request URL</li>
     * <li>stepNumber 5: Validate the Management Server Connection request Username</li>
     * </ol>
     * 
     * @author Sathurya Ravi
     * @refactor Athira
     * 
     * @param device
     *            Dut to be used for execution
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
	     * stepNumber 1: Execute get Parameter on Advanced Management parameters of Device and get the response
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
     * <li>5:Perform Webpa command to validate ACS Management connection request port is not null</li>
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
	     * STEP 5:Perform Webpa command to validate ACS Management connection request port is not null
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
     * <li>stepNumber 1: Execute get Parameter on Device.WiFi.SSID.10001.SSID to get the current 2.4 GHZ private WiFi
     * SSID.</li>
     * <li>stepNumber 2: Execute set parameter on Device.WiFi.SSID.10001.SSID with a new SSID value.</li>
     * <li>stepNumber 3: Verify the new SNMP get on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 for new SSID.</li>
     * <li>stepNumber 4: Verify the new SSID from WebPa using the parameter Device.WiFi.SSID.10001.SSID</li>
     * <li>stepNumber 5: Execute get Parameter on Device.WiFi.SSID.10001.SSID and verify whether the new SSID is being
     * returned</li>
     * <li>stepNumber 6: Execute get Parameter on Device.WiFi.SSID.10101.SSID to get the current 5 GHZ private WiFi
     * SSID.</li>
     * <li>stepNumber 7: Execute set parameter on Device.WiFi.SSID.10101.SSID with a new SSID value.</li>
     * <li>stepNumber 8: Verify the new SNMP get on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101 for new SSID.</li>
     * <li>stepNumber 9: Verify the new SSID from WebPa using the parameter Device.WiFi.SSID.10101.SSID</li>
     * <li>stepNumber 10: Execute get Parameter on Device.WiFi.SSID.10101.SSID and verify whether the new SSID is being
     * returned</li>
     * </ol>
     * 
     * @author Sathurya Ravi
     * @refactor Govardhan
     * 
     * @param device
     *            Dut to be used for execution
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
	     * stepNumber 1: Execute get Parameter on Device.WiFi.SSID.10001.SSID to get the current 2.4 GHZ private
	     * WiFi SSID.
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
	     * stepNumber 2: Execute set parameter on Device.WiFi.SSID.10001.SSID with a new SSID value.
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
	     * stepNumber 3: Execute SNMP get on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 to get 2.4 GHZ private
	     * WiFi SSID.
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
	     * stepNumber 4: Verify the new SSID from WebPa using the parameter Device.WiFi.SSID.10001.SSID
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
	     * stepNumber 5: Execute get Parameter on Device.WiFi.SSID.10001.SSID and verify whether the new SSID is
	     * being returned.
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
	     * stepNumber 6: Execute get Parameter on Device.WiFi.SSID.10101.SSID to get the current 5 GHZ private WiFi
	     * SSID.
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
	     * stepNumber 7: Execute set parameter on Device.WiFi.SSID.10101.SSID with a new SSID value.
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
	     * stepNumber 8: Execute SNMP get on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10101 to get 5 GHZ private
	     * WiFi SSID.
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
	     * stepNumber 9: Verify the new SSID from WebPa using the parameter Device.WiFi.SSID.10101.SSID
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
	     * stepNumber 10: Execute get Parameter on Device.WiFi.SSID.10101.SSID and verify whether the new SSID is
	     * being returned.
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
     * <li>stepNumber 1: Execute set Parameter on Device.WiFi.Radio.10000.Enable(2.4 GHZ) to False</li>
     * <li>stepNumber 2: Verify from WebPa that 2.4 GHZ radio is disabled</li>
     * <li>stepNumber 3: Verify from SNMP that 2.4 GHZ radio is disabled</li>
     * <li>stepNumber 4: Execute get Parameter on the Device.WiFi.Radio.10000.Enable and verify that it is false
     * <li>stepNumber 5: Execute get Parameter on Device.WiFi.Radio.10000.Status and verify that it is down
     * <li>stepNumber 6: Execute get Parameter on Device.WiFi.Radio.10000.LastChange and verify that it shows
     * appropriate time in seconds</li>
     * <li>stepNumber 7: Execute set Parameter on Device.WiFi.Radio.10000.Enable (2.4 GHZ) to True</li>
     * <li>stepNumber 8: Verify from WebPa that 2.4 GHZ radio is enabled</li>
     * <li>stepNumber 9: Verify from SNMP that 2.4 GHZ radio is enabled</li>
     * <li>stepNumber 10: Execute get Parameter on the Device.WiFi.Radio.10000.Enable and verify that it is true
     * <li>stepNumber 11: Execute get Parameter on Device.WiFi.Radio.10000.Status and verify that it is Up
     * <li>stepNumber 12: Execute get Parameter on Device.WiFi.Radio.10000.LastChange and verify that it shows
     * appropriate time in seconds</li>
     * <li>stepNumber 13: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5 GHZ) to False</li>
     * <li>stepNumber 14: Verify from WebPa that 5 GHZ radio is disabled</li>
     * <li>stepNumber 15: Verify from SNMP that 2.4 GHZ radio is disabled</li>
     * <li>stepNumber 16: Execute get Parameter on the Device.WiFi.Radio.10100.Enable and verify that it is false
     * <li>stepNumber 17: Execute get Parameter on Device.WiFi.Radio.10100.Status and verify that it is down
     * <li>stepNumber 18: Execute get Parameter on Device.WiFi.Radio.10100.LastChange and verify that it shows
     * appropriate time in seconds</li>
     * <li>stepNumber 19: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5 GHZ) to True</li>
     * <li>stepNumber 20: Verify from WebPa that 5 GHZ radio is enabled</li>
     * <li>stepNumber 21: Verify from SNMP that 5 GHZ radio is enabled</li>
     * <li>stepNumber 22: Execute get Parameter on the Device.WiFi.Radio.10100.Enable and verify that it is true
     * <li>stepNumber 23: Execute get Parameter on Device.WiFi.Radio.10100.Status and verify that it is Up
     * <li>stepNumber 24: Execute get Parameter on Device.WiFi.Radio.10100.LastChange and verify that it shows
     * appropriate time in seconds</li>
     * </ol>
     * 
     * @author Sathurya Ravi
     * @refactor Rakesh C N, Sruthi Santhosh
     * @param device
     *            device to be used for execution
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
	     * stepNumber 1: Execute set Parameter on Device.WiFi.Radio.10000.Enable (2.4 GHZ) to False
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
	     * stepNumber 4: Execute get Parameter on the Device.WiFi.Radio.10000.Enable and verify that it is false
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
	     * stepNumber 5: Execute get Parameter on Device.WiFi.Radio.10000.Status and verify that it is down
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
	     * stepNumber 6: Execute get Parameter on Device.WiFi.Radio.10000.LastChange and verify that it shows
	     * appropriate time in seconds
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
	     * stepNumber 7: Execute set Parameter on Device.WiFi.Radio.10000.Enable (2.4 GHZ) to True
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
	     * stepNumber 10: Execute get Parameter on the Device.WiFi.Radio.10000.Enable and verify that it is true
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
	     * stepNumber 11: Execute get Parameter on Device.WiFi.Radio.10000.Status and verify that it is up
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
	     * stepNumber 12: Execute get Parameter on Device.WiFi.Radio.10000.LastChange and verify that it shows
	     * appropriate time in seconds
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
	     * stepNumber 13: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5 GHZ) to False
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
	     * stepNumber 16: Execute get Parameter on the Device.WiFi.Radio.10100.Enable and verify that it is false
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
	     * stepNumber 17: Execute get Parameter on Device.WiFi.Radio.10100.Status and verify that it is down
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
	     * stepNumber 18: Execute get Parameter on Device.WiFi.Radio.10100.LastChange and verify that it shows
	     * appropriate time in seconds
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
	     * stepNumber 19: Execute set Parameter on Device.WiFi.Radio.10100.Enable (5 GHZ) to True
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
	     * stepNumber 22: Execute get Parameter on the Device.WiFi.Radio.10100.Enable and verify that it is true
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
	     * stepNumber 23: Execute get Parameter on Device.WiFi.Radio.10100.Status and verify that it is up
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
	     * stepNumber 24: Execute get Parameter on Device.WiFi.Radio.10100.LastChange and verify that it shows
	     * appropriate time in seconds
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

    /**
     * 
     * This method verifies whether WiFi SSID objects can be enabled disabled via TR69
     * 
     * <ol>
     * <li>stepNumber 1: Execute set Parameter on Device.WiFi.SSID.10001.Enable (2.4 GHZ private SSID) to False</li>
     * <li>stepNumber 2: Verify using SNMP the private 2.4GHZ SSID is disabled using
     * .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001</li>
     * <li>stepNumber 3: Verify from WebPa using the parameter Device.WiFi.SSID.10001.Enable that the 2.4 GHZ SSID is
     * disabled</li>
     * <li>stepNumber 4: Execute get Parameter on the Device.WiFi.SSID.10001.Enable and verify that it is false</li>
     * <li>stepNumber 5: Execute get Parameter on Device.WiFi.SSID.10001.Status and verify that it is down</li>
     * <li>stepNumber 6: Execute get Parameter on Device.WiFi.SSID.10001.LastChange and verify that it shows appropriate
     * time in seconds</li>
     * <li>stepNumber 7: Execute set Parameter on Device.WiFi.SSID.10001.Enable (2.4 GHZ private SSID) to True</li>
     * <li>stepNumber 8: Verify using SNMP the private 2.4GHZ SSID is disabled using
     * .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001</li>
     * <li>stepNumber 9: Verify from WebPa using the parameter Device.WiFi.SSID.10001.Enable that the 2.4 GHZ SSID is
     * disabled</li>
     * <li>stepNumber 10: Execute get Parameter on the Device.WiFi.SSID.10001.Enable and verify that it is true</li>
     * <li>stepNumber 11: Execute get Parameter on Device.WiFi.SSID.10001.Status and verify that it is Up</li>
     * <li>stepNumber 12: Execute get Parameter on Device.WiFi.SSID.10001.LastChange and verify that it shows
     * appropriate time in seconds</li>
     * <li>stepNumber 13: Execute set Parameter on Device.WiFi.SSID.10101.Enable (5 GHZ private SSID) to false</li>
     * <li>stepNumber 14: Verify using SNMP the private 5GHZ SSID is disabled using
     * .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101</li>
     * <li>stepNumber 15: Verify from WebPa using the parameter Device.WiFi.SSID.10101.Enable that the 5 GHZ SSID is
     * disabled</li>
     * <li>stepNumber 16: Execute get Parameter on the Device.WiFi.SSID.10101.Enable and verify that it is false</li>
     * <li>stepNumber 17: Execute get Parameter on Device.WiFi.SSID.10101.Status and verify that it is down</li>
     * <li>stepNumber 18: Execute get Parameter on Device.WiFi.SSID.10101.LastChange and verify that it shows
     * appropriate time in seconds</li>
     * <li>stepNumber 19: Execute set Parameter on Device.WiFi.SSID.10101.Enable (5 GHZ private SSID) to True</li>
     * <li>stepNumber 20: Verify using SNMP the private 5GHZ SSID is enabled using
     * .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101</li>
     * <li>stepNumber 21: Verify from WebPa using the parameter Device.WiFi.SSID.10101.Enable that the 5 GHZ SSID is
     * enabled</li>
     * <li>stepNumber 22: Execute get Parameter on the Device.WiFi.SSID.10101.Enable and verify that it is true</li>
     * <li>stepNumber 23: Execute get Parameter on Device.WiFi.SSID.10101.Status and verify that it is Up</li>
     * <li>stepNumber 24: Execute get Parameter on Device.WiFi.SSID.10101.LastChange and verify that it shows
     * appropriate time in seconds</li>
     * </ol>
     * 
     * @author Sathurya Ravi
     * @refactor Govardhan
     * 
     * @param device
     *            Dut to be used for execution
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1008")

    public void testVerifyTR69PrivateWifiSSIDEnableDisable(Dut device) {

	// stores the test result
	boolean status = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-008";
	// stores the error message
	String errorMessage = null;
	// stores the stepNumber
	String stepNumber = null;
	// stores the device model
	String model = null;
	// stores the command
	String command = null;
	// stores the command response
	String response = null;
	// stores last change in seconds for radio
	int lastChange = 0;
	// stores start time in milliseconds
	long startTimeStamp = 0;

	List<Parameter> set_Param = new ArrayList<Parameter>();
	List<String> get_Param = new ArrayList<String>();

	try {

	    /*
	     * stepNumber 1: Execute set Parameter on Device.WiFi.SSID.10001.Enable (2.4 GHZ private SSID) to False
	     */
	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: Disable 2.4 GHZ private SSID from TR69 ");
	    LOGGER.info(
		    "STEP 1: ACTION: set TR69 parameter Device.WiFi.SSID.10001.Enable (2.4 GHZ private SSID) to False to disable 2.4 GHZ private SSID ");
	    LOGGER.info("STEP 1: EXPECTED: The parameter should get set without any error ");
	    LOGGER.info(
		    "******************************************************************************************************************************");
	    stepNumber = "s1";
	    status = false;
	    set_Param.clear();
	    set_Param = BroadBandTr69Utils.setParameterForTr69Set(device, TR69ParamDataType.BOOLEAN.get(),
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED,
		    RDKBTestConstants.FALSE);
	    response = tapEnv.setTR69ParameterValues(device, set_Param);
	    startTimeStamp = System.currentTimeMillis();
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(RDKBTestConstants.STRING_CONNECTION_STATUS);

	    errorMessage = "Not able to set false for 2.4 GHz private SSID using Device.WiFi.SSID.10001.Enable tr69 params. Actual: "
		    + response;
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Able to Disable 2.4 GHZ private SSID from TR69");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    // Introducing wait time to make sure the operation is completed
	    // before further validation

	    Thread.sleep(100000);

	    /*
	     * stepNumber 2: Verify using SNMP the private 2.4GHZ SSID is disabled using
	     * .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001
	     */
	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION: validate via SNMP that 2.4 GHZ private SSID is disabled ");
	    LOGGER.info(
		    "STEP 2: ACTION: Verify using SNMP the private 2.4GHZ SSID is disabled using  .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 ");
	    LOGGER.info("STEP 2: EXPECTED: The MIB should return an integer 2");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s2";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getOid(),
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getTableIndex());

	    status = CommonMethods.isNotNull(response)
		    && (Integer.parseInt(response) == BroadBandTestConstants.CONSTANT_2);

	    errorMessage = "The MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 has not returned the expected value. Actual: "
		    + response;

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL :  Able to disable via 2.4 GHZ private via SNMP");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 3: Verify from WebPa using the parameter Device.WiFi.SSID.10001.Enable that the 2.4 GHZ SSID
	     * is disabled
	     */
	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: validate via WebPa that 2.4 GHZ private SSID is disabled ");
	    LOGGER.info(
		    "STEP 3: ACTION: Verify using WebPa the private 2.4GHZ SSID is disabled using Device.WiFi.SSID.10001.Enable ");
	    LOGGER.info("STEP 3: EXPECTED: The response should return false");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s3";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED);
	    if (null != response)
		status = response.equalsIgnoreCase(RDKBTestConstants.FALSE);

	    errorMessage = "The Webpa parameter Device.WiFi.SSID.10001.Enable has not returned the expected value. Actual: "
		    .concat(response);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL :  Able to disable via 2.4 GHZ private via webpa");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 4: Execute get Parameter on the Device.WiFi.SSID.10001.Enable and verify that it is false
	     */
	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION: Execute get Parameter on the Device.WiFi.SSID.10001.Enable and verify that it is false ");
	    LOGGER.info("STEP 4: ACTION: Verify from the Device.WiFi.SSID.10001.Enable that 5 GHZ is false ");
	    LOGGER.info("STEP 4: EXPECTED: The parameters should return the false");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s4";
	    status = false;
	    get_Param.clear();
	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED);
		response = tapEnv.getTR69ParameterValues(device, get_Param);
		status = response.equalsIgnoreCase(RDKBTestConstants.FALSE);

	    } catch (Exception exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10001.Enable did not return the expected value. Actual value: "
		    + response;

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL :  The parameter Device.WiFi.SSID.10001.Enable did return the expected value");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 5: Execute get Parameter on Device.WiFi.Radio.10100.Status and verify that it is down
	     */
	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION: Execute get Parameter on Device.WiFi.SSID.10001.Status and verify that it is down ");
	    LOGGER.info("STEP 5: ACTION: Verify from the Device.WiFi.SSID.10001.Status that 5 GHZ is down ");
	    LOGGER.info("STEP 5: EXPECTED: The parameters should return the value down");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s5";
	    status = false;
	    get_Param.clear();
	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);
		response = tapEnv.getTR69ParameterValues(device, get_Param);
		if (null != response)
		    status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_DOWN);

	    } catch (Exception exception) {
		response = exception.getMessage();
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10001.Status did not return the expected value. Actual value: "
		    + response;

	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL :  The parameter Device.WiFi.SSID.10001.Status did return the expected value");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 6: Execute get Parameter on Device.WiFi.SSID.10001.LastChange and verify that it shows
	     * appropriate time in seconds
	     */
	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION: Execute get Parameter on Device.WiFi.SSID.10001.LastChange and verify that it shows appropriate time in seconds ");
	    LOGGER.info(
		    "STEP 6: ACTION: Verify from the Device.WiFi.SSID.10001.LastChange that last change value is valid");
	    LOGGER.info("STEP 6: EXPECTED: The parameters should valid time in seconds");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s6";
	    status = false;
	    get_Param.clear();
	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LAST_CHANGE_FOR_2_4GHZ_PRIVATE_SSID);
		lastChange = Integer.parseInt(tapEnv.getTR69ParameterValues(device, get_Param));

		// Adding 10 seconds extra to (lastChange <
		// (System.currentTimeMillis() - startTimeStamp) to get the
		// approximate seconds value
		status = lastChange < ((System.currentTimeMillis() - startTimeStamp) + 10000);

	    } catch (NumberFormatException exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10001.LastChange did not return the expected value. Actual value: "
		    + (System.currentTimeMillis() - startTimeStamp) + 10000;

	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL :  The parameter Device.WiFi.SSID.10001.LastChange did return the expected value");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 7: Execute set Parameter on Device.WiFi.SSID.10001.Enable (2.4 GHZ private SSID) to True
	     */
	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION: Disable 2.4 GHZ private SSID from TR69 ");
	    LOGGER.info(
		    "STEP 7: ACTION: set TR69 parameter Device.WiFi.SSID.10001.Enable (2.4 GHZ private SSID) to True to disable 2.4 GHZ private SSID ");
	    LOGGER.info("STEP 7: EXPECTED: The parameter should get set without any error ");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s7";
	    status = false;

	    errorMessage = "Not able to set true for 2.4 GHz radio using Device.WiFi.SSID.10001.Enable tr69 params ";
	    set_Param = BroadBandTr69Utils.setParameterForTr69Set(device, TR69ParamDataType.BOOLEAN.get(),
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED,
		    RDKBTestConstants.TRUE);

	    response = tapEnv.setTR69ParameterValues(device, set_Param);

	    startTimeStamp = System.currentTimeMillis();

	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(RDKBTestConstants.STRING_CONNECTION_STATUS);

	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : Able to set true for 2.4 GHz radio using Device.WiFi.SSID.10001.Enable tr69 params");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    // Introducing wait time to make sure the operation is completed
	    // before further validation

	    Thread.sleep(100000);

	    /*
	     * stepNumber 8: Verify using SNMP the private 2.4GHZ SSID is enabled using
	     * .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001
	     */
	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION: validate via SNMP that 2.4 GHZ private SSID is enabled ");
	    LOGGER.info(
		    "STEP 8: ACTION: Verify using SNMP the private 2.4GHZ SSID is disabled using  .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 ");
	    LOGGER.info("STEP 8: EXPECTED: The MIB should return an integer 1");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s8";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getOid(),
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getTableIndex());

	    status = CommonMethods.isNotNull(response)
		    && (Integer.parseInt(response) == BroadBandTestConstants.CONSTANT_1);

	    errorMessage = "The MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 has not returned the expected value. Actual: "
		    + response;

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : MIB has returned the expected value");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 9: Verify from WebPa using the parameter Device.WiFi.SSID.10001.Enable that the 2.4 GHZ SSID
	     * is enabled
	     */
	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION: validate via WebPa that 2.4 GHZ private SSID is enabled ");
	    LOGGER.info(
		    "STEP 9: ACTION: Verify using WebPa the private 2.4GHZ SSID is enabled using Device.WiFi.SSID.10001.Enable ");
	    LOGGER.info("STEP 9: EXPECTED: The response should return true");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s9";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED);
	    if (null != response)
		status = response.equalsIgnoreCase(RDKBTestConstants.TRUE);

	    errorMessage = "The WebPa parameter Device.WiFi.SSID.10001.Enable has not returned the expected value. Actual value: "
		    .concat(response);

	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : The WebPa parameter Device.WiFi.SSID.10001.Enable has returned the expected value.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 10: Execute get Parameter on the Device.WiFi.SSID.10001.Enable and verify that it is true
	     */
	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION: Execute get Parameter on the Device.WiFi.SSID.10001.Enable and verify that it is true ");
	    LOGGER.info("STEP 10: ACTION: Verify from the Device.WiFi.SSID.10001.Enable that 5 GHZ is true ");
	    LOGGER.info("STEP 10: EXPECTED: The parameters should return the true");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s10";
	    status = false;

	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED);
		response = tapEnv.getTR69ParameterValues(device, get_Param);

		status = response.equalsIgnoreCase(RDKBTestConstants.TRUE);

	    } catch (Exception exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10001.Enable did not return the expected value. Actual value: "
		    + response;

	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : The parameter Device.WiFi.SSID.10001.Enable did return the expected value.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 11: Execute get Parameter on Device.WiFi.SSID.10001.Status and verify that it is up
	     */

	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION: Execute get Parameter on Device.WiFi.SSID.10001.Status and verify that it is up ");
	    LOGGER.info("STEP 11: ACTION: Verify from the Device.WiFi.SSID.10001.Status that 5 GHZ is up ");
	    LOGGER.info("STEP 11: EXPECTED: The parameters should return the value up");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s11";
	    status = false;

	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);
		response = tapEnv.getTR69ParameterValues(device, get_Param);

		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_UP);

	    } catch (Exception exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter did not return the expected value. Actual value: " + response;

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : The parameter did return the expected value.");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 12: Execute get Parameter on Device.WiFi.SSID.10001.LastChange and verify that it shows
	     * appropriate time in seconds
	     */

	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION: Execute get Parameter on Device.WiFi.SSID.10001.LastChange and verify that it shows appropriate time in seconds ");
	    LOGGER.info(
		    "STEP 12: ACTION: Verify from the Device.WiFi.SSID.10001.LastChange that last change value is valid");
	    LOGGER.info("STEP 12: EXPECTED: The parameters should valid time in seconds");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s12";
	    status = false;

	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LAST_CHANGE_FOR_2_4GHZ_PRIVATE_SSID);
		lastChange = Integer.parseInt(tapEnv.getTR69ParameterValues(device, get_Param));

		// Adding 10 seconds extra to (lastChange <
		// (System.currentTimeMillis() - startTimeStamp) to get the
		// approximate seconds value
		status = lastChange < ((System.currentTimeMillis() - startTimeStamp) + 10000);

	    } catch (NumberFormatException exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10001.LastChange did not return the expected value. Actual value: "
		    + (System.currentTimeMillis() - startTimeStamp) + 10000;

	    if (status) {
		LOGGER.info(
			"STEP 12: ACTUAL : The parameter Device.WiFi.SSID.10001.LastChange did return the expected value.");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 13: Execute set Parameter on Device.WiFi.SSID.10101.Enable (5 GHZ private SSID) to False
	     */

	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION: Disable 5 GHZ private SSID from TR69 ");
	    LOGGER.info(
		    "STEP 13: ACTION: set TR69 parameter Device.WiFi.SSID.10101.Enable (5 GHZ private SSID) to False to disable 5 GHZ private SSID ");
	    LOGGER.info("STEP 13: EXPECTED: The parameter should get set without any error ");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s13";
	    status = false;
	    set_Param = BroadBandTr69Utils.setParameterForTr69Set(device, TR69ParamDataType.BOOLEAN.get(),
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED,
		    RDKBTestConstants.FALSE);

	    response = tapEnv.setTR69ParameterValues(device, set_Param);

	    startTimeStamp = System.currentTimeMillis();

	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(RDKBTestConstants.STRING_CONNECTION_STATUS);

	    errorMessage = "Not able to set false for 5 GHz radio using Device.WiFi.SSID.10101.Enable tr69 params. Actual: "
		    + response;

	    if (status) {
		LOGGER.info(
			"STEP 13: ACTUAL : Able to set false for 5 GHz radio using Device.WiFi.SSID.10101.Enable tr69 params.");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    // Introducing wait time to make sure the operation is completed
	    // before further validation

	    Thread.sleep(100000);

	    /*
	     * stepNumber 14: Verify using SNMP the private 5GHZ SSID is disabled using
	     * .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101
	     */

	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION: validate via SNMP that 5 GHZ private SSID is disabled ");
	    LOGGER.info(
		    "STEP 14: ACTION: Verify using SNMP the private 5 GHZ SSID is disabled using  .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101 ");
	    LOGGER.info("STEP 14: EXPECTED: The MIB should return an integer 2");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s14";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getOid(),
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex());

	    status = CommonMethods.isNotNull(response)
		    && (Integer.parseInt(response) == BroadBandTestConstants.CONSTANT_2);

	    errorMessage = "The MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101 has not returned the expected value. Actual: "
		    + status;

	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : MIB has returned the expected value");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 15: Verify from WebPa using the parameter Device.WiFi.SSID.10101.Enable that the 5 GHZ SSID is
	     * disabled
	     */

	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION: validate via WebPa that 5 GHZ private SSID is disabled ");
	    LOGGER.info(
		    "STEP 15: ACTION: Verify using WebPa the private 5 GHZ SSID is disabled using Device.WiFi.SSID.10101.Enable ");
	    LOGGER.info("STEP 15: EXPECTED: The response should return false");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s15";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED);

	    if (null != response)
		status = response.equalsIgnoreCase(RDKBTestConstants.FALSE);

	    errorMessage = "The WebPa parameter Device.WiFi.SSID.10101.Enable has not returned the expected value. Actual value: "
		    .concat(response);

	    if (status) {
		LOGGER.info(
			"STEP 15: ACTUAL : The WebPa parameter Device.WiFi.SSID.10101.Enable has returned the expected value");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 16: Execute get Parameter on the Device.WiFi.SSID.10101.Enable and verify that it is false
	     */

	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 16: DESCRIPTION: Execute get Parameter on the Device.WiFi.SSID.10101.Enable and verify that it is false ");
	    LOGGER.info("STEP 16: ACTION: Verify from the Device.WiFi.SSID.10101.Enable that 5 GHZ is false ");
	    LOGGER.info("STEP 16: EXPECTED: The parameters should return the false");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s16";
	    status = false;

	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED);
		response = tapEnv.getTR69ParameterValues(device, get_Param);

		status = response.equalsIgnoreCase(RDKBTestConstants.FALSE);

	    } catch (Exception exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10101.Enable did not return the expected value. Actual value: "
		    + response;

	    if (status) {
		LOGGER.info(
			"STEP 16: ACTUAL : The parameter Device.WiFi.SSID.10101.Enable did return the expected value.");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 17: Execute get Parameter on Device.WiFi.SSID.10101.Status and verify that it is down
	     */
	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 17: DESCRIPTION: Execute get Parameter on Device.WiFi.SSID.10101.Status and verify that it is down ");
	    LOGGER.info("STEP 17: ACTION: Verify from the Device.WiFi.SSID.10101.Status that 5 GHZ is down ");
	    LOGGER.info("STEP 17: EXPECTED: The parameters should return the value down");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s17";
	    status = false;

	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);
		response = tapEnv.getTR69ParameterValues(device, get_Param);

		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_DOWN);

	    } catch (Exception exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10101.Status did not return the expected value. Actual value: "
		    + response;

	    if (status) {
		LOGGER.info(
			"STEP 17: ACTUAL : The parameter Device.WiFi.SSID.10101.Status did return the expected value");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 18: Execute get Parameter on Device.WiFi.SSID.10101.LastChange and verify that it shows
	     * appropriate time in seconds
	     */

	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 18: DESCRIPTION: Execute get Parameter on Device.WiFi.SSID.10101.LastChange and verify that it shows appropriate time in seconds ");
	    LOGGER.info(
		    "STEP 18: ACTION: Verify from the Device.WiFi.SSID.10101.LastChange that last change value is valid");
	    LOGGER.info("STEP 18: EXPECTED: The parameters should valid time in seconds");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s18";
	    status = false;

	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LAST_CHANGE_FOR_5GHZ_PRIVATE_SSID);
		lastChange = Integer.parseInt(tapEnv.getTR69ParameterValues(device, get_Param));

		// Adding 10 seconds extra to (lastChange <
		// (System.currentTimeMillis() - startTimeStamp) to get the
		// approximate seconds value
		status = lastChange < (System.currentTimeMillis() - startTimeStamp) + 10000;

	    } catch (NumberFormatException exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10101.LastChange did not return the expected value. Actual value: "
		    + (System.currentTimeMillis() - startTimeStamp) + 10000;

	    if (status) {
		LOGGER.info(
			"STEP 18: ACTUAL : The parameter Device.WiFi.SSID.10101.LastChange did return the expected value");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 19: Execute set Parameter on Device.WiFi.SSID.10101.Enable (5 GHZ private SSID) to True
	     */

	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION: Enable 5 GHZ private SSID from TR69 ");
	    LOGGER.info(
		    "STEP 19: ACTION: set TR69 parameter Device.WiFi.SSID.10101.Enable (5 GHZ private SSID) to True to enable 5 GHZ private SSID ");
	    LOGGER.info("STEP 19: EXPECTED: The parameter should get set without any error ");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s19";
	    status = false;

	    set_Param = BroadBandTr69Utils.setParameterForTr69Set(device, TR69ParamDataType.BOOLEAN.get(),
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED, RDKBTestConstants.TRUE);

	    response = tapEnv.setTR69ParameterValues(device, set_Param);
	    startTimeStamp = System.currentTimeMillis();

	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(RDKBTestConstants.STRING_CONNECTION_STATUS);

	    errorMessage = "Not able to set true for 5 GHz radio using Device.WiFi.SSID.10101.Enable tr69 params. Actual: "
		    + status;

	    if (status) {
		LOGGER.info(
			"STEP 19: ACTUAL : Able to set true for 5 GHz radio using Device.WiFi.SSID.10101.Enable tr69 params");
	    } else {
		LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    // Introducing wait time to make sure the operation is completed
	    // before further validation

	    Thread.sleep(100000);

	    /*
	     * stepNumber 20: Verify using SNMP the private 5GHZ SSID is enabled using
	     * .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101
	     */
	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 20: DESCRIPTION: validate via SNMP that 5 GHZ private SSID is enabled ");
	    LOGGER.info(
		    "STEP 20: ACTION: Verify using SNMP the private 5 GHZ SSID is disabled using  .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101 ");
	    LOGGER.info("STEP 20: EXPECTED: The MIB should return an integer 1");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s20";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getOid(),
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex());

	    status = CommonMethods.isNotNull(response)
		    && (Integer.parseInt(response) == BroadBandTestConstants.CONSTANT_1);

	    errorMessage = "The MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10101 has not returned the expected value. Actual: "
		    + status;

	    if (status) {
		LOGGER.info("STEP 20: ACTUAL : MIB has returned the expected value");
	    } else {
		LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 21: Verify from WebPa using the parameter Device.WiFi.SSID.10101.Enable that the 5 GHZ SSID is
	     * enabled
	     */

	    LOGGER.info(
		    "******************************************************************************************************************************");
	    LOGGER.info("STEP 21: DESCRIPTION: validate via WebPa that 5 GHZ private SSID is enabled ");
	    LOGGER.info(
		    "STEP 21: ACTION: Verify using WebPa the private 5 GHZ SSID is enabled using Device.WiFi.SSID.10101.Enable ");
	    LOGGER.info("STEP 21: EXPECTED: The response should return true");
	    LOGGER.info(
		    "******************************************************************************************************************************");

	    stepNumber = "s21";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED);

	    if (null != response)
		status = response.equalsIgnoreCase(RDKBTestConstants.TRUE);

	    errorMessage = "The WebPa parameter Device.WiFi.SSID.10101.Enable has not returned the expected value. Actual value: "
		    .concat(response);

	    if (status) {
		LOGGER.info(
			"STEP 21: ACTUAL : The WebPa parameter Device.WiFi.SSID.10101.Enable has returned the expected value");
	    } else {
		LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 22: Execute get Parameter on the Device.WiFi.SSID.10101.Enable and verify that it is true
	     */

	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 22: DESCRIPTION: Execute get Parameter on the Device.WiFi.SSID.10101.Enable and verify that it is true ");
	    LOGGER.info("STEP 22: ACTION: Verify from the Device.WiFi.SSID.10101.Enable that 5 GHZ is true ");
	    LOGGER.info("STEP 22: EXPECTED: The parameters should return the true");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s22";
	    status = false;

	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED);
		response = tapEnv.getTR69ParameterValues(device, get_Param);
		status = response.equalsIgnoreCase(RDKBTestConstants.TRUE);

	    } catch (Exception exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10101.Enable did not return the expected value. Actual value: "
		    + response;

	    if (status) {
		LOGGER.info(
			"STEP 22: ACTUAL : The parameter Device.WiFi.SSID.10101.Enable did return the expected value");
	    } else {
		LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 23: Execute get Parameter on Device.WiFi.SSID.10101.Status and verify that it is down
	     */
	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 23: DESCRIPTION: Execute get Parameter on Device.WiFi.SSID.10101.Status and verify that it is up ");
	    LOGGER.info("STEP 23: ACTION: Verify from the Device.WiFi.SSID.10101.Status that 5 GHZ is up ");
	    LOGGER.info("STEP 23: EXPECTED: The parameters should return the value up");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s23";
	    status = false;

	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);
		response = tapEnv.getTR69ParameterValues(device, get_Param);

		status = response.equalsIgnoreCase(BroadBandTestConstants.STRING_UP);

	    } catch (Exception exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10101.Status did not return the expected value. Actual value: "
		    + status;

	    if (status) {
		LOGGER.info(
			"STEP 23: ACTUAL : The parameter Device.WiFi.SSID.10101.Status did return the expected value");
	    } else {
		LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	    /*
	     * stepNumber 24: Execute get Parameter on Device.WiFi.SSID.10101.LastChange and verify that it shows
	     * appropriate time in seconds
	     */
	    LOGGER.info(
		    "***************************************************************************************************************");
	    LOGGER.info(
		    "STEP 24: DESCRIPTION: Execute get Parameter on Device.WiFi.SSID.10101.LastChange and verify that it shows appropriate time in seconds ");
	    LOGGER.info(
		    "STEP 24: ACTION: Verify from the Device.WiFi.SSID.10101.LastChange that last change value is valid");
	    LOGGER.info("STEP 24: EXPECTED: The parameters should valid time in seconds");
	    LOGGER.info(
		    "****************************************************************************************************************");

	    stepNumber = "s24";
	    status = false;

	    try {
		get_Param = BroadBandTr69Utils.getParameterForTr69Get(
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LAST_CHANGE_FOR_5GHZ_PRIVATE_SSID);
		lastChange = Integer.parseInt(tapEnv.getTR69ParameterValues(device, get_Param));

		// Adding 10 seconds extra to (lastChange <
		// (System.currentTimeMillis() - startTimeStamp) to get the
		// approximate seconds value
		status = lastChange < (System.currentTimeMillis() - startTimeStamp) + 10000;

	    } catch (NumberFormatException exception) {
		LOGGER.error(exception.getMessage());
	    }

	    errorMessage = "The parameter Device.WiFi.SSID.10101.LastChange did not return the expected value. Actual value: "
		    + (System.currentTimeMillis() - startTimeStamp) + 10000;

	    if (status) {
		LOGGER.info(
			"STEP 24: ACTUAL : The parameter Device.WiFi.SSID.10101.LastChange did return the expected value.");
	    } else {
		LOGGER.error("STEP 24: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating WiFi SSID Enable/Disable from TR69 " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, errorMessage, true);
	}
    }

    /**
     *
     * This method verifies the expected TR69 functionality with default values, changed values and after reboot values
     * of Device ManagementServer Passwords.
     * 
     * <ol>
     * <li>Pre condition: Perform factory reset on the device</li>
     * <li>Step 1: Verify encrypted Device ManagementServer STUN Password is not stored after factory reset</li>
     * <li>Step 2: Verify TR-69 process is running on the device.</li>
     * <li>Step 3: Verify TR-69 is enabled, if not, enable it.</li>
     * <li>Step 4: Verify value of Device ManagementServer Password has been set.</li>
     * <li>Step 5: Verify value of Device ManagementServer ConnectionRequestPassword has been set.</li>
     * <li>Step 6: Verify default value of Device ManagementServer ConnectionRequestPassword</li>
     * <li>Step 7: Verify default value of Device ManagementServer Password</li>
     * <li>Step 8: Verify default value of Device ManagementServer STUNPassword</li>
     * <li>Step 9: Execute any TR69 query.</li>
     * <li>Step 10: Change value of Device ManagementServer Password</li>
     * <li>Step 11: Execute any TR69 query.</li>
     * <li>Step 12: Reboot the device.</li>
     * <li>Step 13: Verify value of Device ManagementServer STUNPassword persists persists after reboot</li>
     * <li>Step 14: Verify value of Device ManagementServer Password persists after reboot</li>
     * <li>Step 15: Verify value of Device ManagementServer ConnectionRequestPassword persists after reboot</li>
     * <li>Step 16: Execute any TR69 query.</li>
     * <li>Step 17: Reset default value of Device ManagementServer Password</li>
     * <li>Step 18: Execute any TR69 query.</li>
     * <li>Post conditions:</li>
     * <li>1. Reset default value of Device ManagementServer Password</li>
     * <li>2. Reactivate device after factory reset</li>
     * </ol>
     * 
     * @author Ashwin Sankarasubramanian
     * @refactor Govardhan
     * 
     * @param device
     *            Dut to be used for execution
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1005")
    public void testVerifyTR69PasswordsFunctionality(Dut device) {

	// stores the test result
	boolean result = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-005";
	// stores the error message
	String errorMessage = null;
	// stores the command response
	String response = null;
	// stores the test step number
	String step = "s1";
	// stores the default value of Device.ManagementServer.Password
	String defaultMgmtPwd = null;
	// stores parameter list for TR69 REST API query
	String[] params = { BroadBandWebPaConstants.TR69_PARAM_SERIAL_NUMBER };
	// stores the start of poll duration
	long startTime = 0;

	List<String> get_param = new ArrayList<String>();

	try {
	    LOGGER.info("STARTING TEST CASE : TC-RDKB-TR69-1005");
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("Pre condition: Perform factory reset on the device");
	    LOGGER.info("*****************************************************************************************");

	    result = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
	    LOGGER.info("Result of factory reset using webpa: " + result);
	    if (!result) {
		result = BroadBandCommonUtils.performFactoryResetSnmp(tapEnv, device);
		LOGGER.info("Result of factory reset using snmp: " + result);
	    }
	    if (!result) {
		errorMessage = "Device Factory Reset could not be performed as a part of pre-condition.";
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    /*
	     * Step 1: Verify Device ManagementServer STUN Password is not stored after factory reset
	     */
	    step = "s1";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify Device Device ManagementServer STUN Password is not stored after factory reset ");
	    LOGGER.info("STEP 1: ACTION : Search found MgmtSTUNCRPwdID stored after factory reset in location "
		    + BroadBandCommandConstants.FILE_NVRAM_KEYS_MGMTSTUNCRPWDID);
	    LOGGER.info("STEP 1: EXPECTED : Device ManagementServer STUN Password is not stored after factory reset");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Search found MgmtSTUNCRPwdID stored after factory reset in location: "
		    + BroadBandCommandConstants.FILE_NVRAM_KEYS_MGMTSTUNCRPWDID;
	    result = !CommonUtils.isFileExists(device, tapEnv,
		    BroadBandCommandConstants.FILE_NVRAM_KEYS_MGMTSTUNCRPWDID);
	    if (result) {
		LOGGER.info(
			"STEP 1: ACTUAL : Device ManagementServer STUN Password is not stored after factory reset ");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 2: Verify TR-69 process is running on the device.
	     */
	    step = "s2";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify TR-69 process is running on the device.");
	    LOGGER.info("STEP 2: ACTION : get Pid of the TR69 process");
	    LOGGER.info("STEP 2: EXPECTED : TR-69 process is running on the device.");
	    LOGGER.info("**********************************************************************************");

	    startTime = System.currentTimeMillis();
	    errorMessage = "TR-69 process is not running";
	    while (System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !result
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
		result = CommonMethods.isNotNull(
			CommonMethods.getPidOfProcess(device, tapEnv, StbProcess.CCSP_TR069.getProcessName()));
	    }
	    if (result) {
		LOGGER.info("STEP 2: ACTUAL : TR-69 process is running on the device.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 3: Verify TR-69 is enabled, if not, enable it.
	     */
	    step = "s3";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify TR-69 is enabled, if not, enable it.");
	    LOGGER.info("STEP 3: ACTION : Enable TR69 using webpa paramater "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_ENABLECWMP);
	    LOGGER.info("STEP 3: EXPECTED : TR-69 is enabled");
	    LOGGER.info("**********************************************************************************");
	    startTime = System.currentTimeMillis();
	    errorMessage = "Unable to enable TR-69 using webpa request";
	    while (System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !result
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
		result = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_ENABLECWMP,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
			BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
	    }
	    if (result) {
		LOGGER.info("STEP 3: ACTUAL : TR-69 is enabled");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 4: Verify value of Device ManagementServer Password has been set.
	     */
	    step = "s4";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify value of Device ManagementServer Password has been set.");
	    LOGGER.info("STEP 4: ACTION : Device ManagementServer Password in ManagementServerPasswordID");
	    LOGGER.info("STEP 4: EXPECTED : Verify value of Device ManagementServer Password has been set.");
	    LOGGER.info("**********************************************************************************");

	    startTime = System.currentTimeMillis();
	    while (System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !result
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
		errorMessage = "Device ManagementServer Password not set after ACS connection";
		if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_MGMTPWDID_CHANGED,
			BroadBandTestConstants.RDKLOGS_LOGS_TR69LOG_TXT_0))) {
		    errorMessage = "Unable to find encrypted ManagementServer Password file stored in location: "
			    + BroadBandCommandConstants.FILE_NVRAM_KEYS_MGMTPWDID;
		    result = CommonUtils.isFileExists(device, tapEnv,
			    BroadBandCommandConstants.FILE_NVRAM_KEYS_MGMTPWDID);
		}
	    }
	    if (result) {
		LOGGER.info("STEP 4: ACTUAL : Value of Device ManagementServer Password has been set");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 5: Verify value of Device ManagementServer ConnectionRequestPassword has been set.
	     */
	    step = "s5";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify value of Device ManagementServer ConnectionRequestPassword has been set.");
	    LOGGER.info(
		    "STEP 5: ACTION : Verify Device ManagementServer Password in ManagementServerConnectionRequestPasswordID");
	    LOGGER.info("STEP 5: EXPECTED : Value of Device ManagementServer ConnectionRequestPassword has been set.");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Device ManagementServer ConnectionRequestPassword not set after ACS connection";
	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_MGMTCRPWDID_CHANGED,
		    BroadBandTestConstants.RDKLOGS_LOGS_TR69LOG_TXT_0))) {
		errorMessage = "Unable to find encrypted ManagementServer ConnectionRequestPassword file stored in location: "
			+ BroadBandCommandConstants.FILE_NVRAM_KEYS_MGMTCRPWDID;
		result = CommonUtils.isFileExists(device, tapEnv,
			BroadBandCommandConstants.FILE_NVRAM_KEYS_MGMTCRPWDID);
	    }
	    if (result) {
		LOGGER.info(
			"STEP 5: ACTUAL : Value of Device ManagementServer ConnectionRequestPassword has been set.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 6: Verify default value of Device ManagementServer Password
	     */
	    step = "s6";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify default value of Device ManagementServer Password");
	    LOGGER.info("STEP 6: ACTION : Verify Device ManagementServer Password using webpa parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_PASSWORD);
	    LOGGER.info("STEP 6: EXPECTED : Device ManagementServer Password value is not null");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Device ManagementServer Password value is null";
	    defaultMgmtPwd = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_PASSWORD);
	    result = CommonMethods.isNotNull(defaultMgmtPwd);
	    if (result) {
		LOGGER.info("STEP 6: ACTUAL : Device ManagementServer Password value is not null");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 7: Verify default value of Device ManagementServer ConnectionRequestPassword
	     */
	    step = "s7";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify default value of Device ManagementServer ConnectionRequestPassword");
	    LOGGER.info(
		    "STEP 7: ACTION : Verify Device ManagementServer ConnectionRequestPassword using webpa parameter "
			    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_CONNECTIONREQUESTPASSWORD);
	    LOGGER.info("STEP 7: EXPECTED : Device ManagementServer ConnectionRequestPassword value is null");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Value of Device ManagementServer ConnectionRequestPassword is not null";
	    result = CommonMethods.isNull(tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_CONNECTIONREQUESTPASSWORD));
	    if (result) {
		LOGGER.info("STEP 7: ACTUAL : Device ManagementServer ConnectionRequestPassword value is null");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 8: Verify default value of Device ManagementServer STUNPassword
	     */
	    step = "s8";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify default value of Device ManagementServer STUNPassword");
	    LOGGER.info("STEP 8: ACTION : Verify Device ManagementServer STUNPassword using webpa parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_STUNPASSWORD);
	    LOGGER.info("STEP 8: EXPECTED : Device ManagementServer STUNPassword value is null");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Value of Device ManagementServer STUNPassword is not null";
	    result = CommonMethods.isNull(tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_STUNPASSWORD));
	    if (result) {
		LOGGER.info("STEP 8: ACTUAL : Device ManagementServer STUNPassword value is null");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 9: Execute any TR69 query.
	     */
	    step = "s9";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Execute any TR69 query.");
	    LOGGER.info("STEP 9: ACTION : Verify TR69 query executes successfully");
	    LOGGER.info("STEP 9: EXPECTED : TR69 query works as expected");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "TR69 query failed to execute successfully";
	    startTime = System.currentTimeMillis();
	    get_param.clear();
	    while (System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !result
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
		response = null;
		try {
		    get_param = BroadBandTr69Utils
			    .getParameterForTr69Get(BroadBandWebPaConstants.TR69_PARAM_SERIAL_NUMBER);
		    response = tapEnv.getTR69ParameterValues(device, get_param);
		} catch (Exception exception) {
		    errorMessage = exception.getMessage();
		}
		LOGGER.info("Expected serial number: " + device.getSerialNumber());
		LOGGER.info("Serial number obtained from TR69 query: " + response);
		result = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(device.getSerialNumber());
	    }
	    if (result) {
		LOGGER.info("STEP 9: ACTUAL : TR69 query works as expected");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 10: Change value of Device ManagementServer Password
	     */
	    step = "s10";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Change value of Device ManagementServer Password");
	    LOGGER.info("STEP 10: ACTION : Verify Device ManagementServer STUNPassword using webpa parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_PASSWORD);
	    LOGGER.info("STEP 10: EXPECTED : Successfully changed value of Device ManagementServer Password");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to change the value of Device ManagementServer Password";
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_PASSWORD,
		    BroadBandTestConstants.CONSTANT_0, BroadBandTestConstants.STRING_NAME);
	    if (result) {
		LOGGER.info("STEP 10: ACTUAL : Successfully changed value of Device ManagementServer Password");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 11: Execute any TR69 query.
	     */
	    step = "s11";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Execute any TR69 query.");
	    LOGGER.info("STEP 11: ACTION : Verify TR69 query executes successfully");
	    LOGGER.info("STEP 11: EXPECTED : TR69 query failed with connection error after changing password");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Tr69 query did not fail with connection error after changing password";
	    try {
		get_param = BroadBandTr69Utils.getParameterForTr69Get(BroadBandWebPaConstants.TR69_PARAM_SERIAL_NUMBER);
		response = tapEnv.getTR69ParameterValues(device, get_param);
	    } catch (Exception exception) {
		response = exception.getMessage();
	    }
	    LOGGER.info("Response obtained from TR69 query: " + response);
	    result = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.STRING_FAILED);
	    if (result) {
		LOGGER.info("STEP 11: ACTUAL : TR69 query failed with connection error after changing password");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 12: Reboot the device
	     */
	    step = "s12";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Reboot the device");
	    LOGGER.info("STEP 12: ACTION : Reboot the device using webpa parameter");
	    LOGGER.info("STEP 12: EXPECTED : Device rebooted successfully");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Device did not reboot successfully";
	    result = CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device);
	    if (result) {
		LOGGER.info("STEP 12: ACTUAL : Device rebooted successfully");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 13: Verify null value of Device ManagementServer STUNPassword persists persists after reboot
	     */
	    step = "s13";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : Verify null value of Device ManagementServer STUNPassword persists after reboot");
	    LOGGER.info("STEP 13: ACTION : verify Device ManagementServer STUNPassword using webpa parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_STUNPASSWORD);
	    LOGGER.info(
		    "STEP 13: EXPECTED : Null value of Device Device ManagementServer STUNPassword persists after reboot");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Value of Device ManagementServer STUNPassword is not null after reboot";
	    result = CommonMethods.isNull(tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_STUNPASSWORD));
	    if (result) {
		LOGGER.info(
			"STEP 13: ACTUAL : Null value of Device Device ManagementServer STUNPassword persists after reboot");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 14: Verify value of Device ManagementServer Password persists after reboot
	     */
	    step = "s14";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION : Verify  value of Device ManagementServer Password persists after reboot");
	    LOGGER.info("STEP 14: ACTION : verify Device ManagementServer Password using webpa parameter "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_PASSWORD);
	    LOGGER.info("STEP 14: EXPECTED : Value of Device ManagementServer Password persists after reboot");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Device ManagementServer Password value is null after reboot";
	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_PASSWORD);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Device ManagementServer Password value changed after reboot";
		result = response.equalsIgnoreCase(BroadBandTestConstants.STRING_NAME);
	    }
	    if (result) {
		LOGGER.info("STEP 14: ACTUAL : Value of Device ManagementServer Password persists after reboot");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 15: Verify null value of Device ManagementServer ConnectionRequestPassword persists after reboot
	     */
	    step = "s15";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION : Verify null value of Device ManagementServer ConnectionRequestPassword persists after reboot");
	    LOGGER.info(
		    "STEP 15: ACTION : verify Device ManagementServer ConnectionRequestPassword using webpa parameter "
			    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_CONNECTIONREQUESTPASSWORD);
	    LOGGER.info(
		    "STEP 15: EXPECTED : Null value of Device ManagementServer ConnectionRequestPassword persists after reboot");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Value of Device ManagementServer ConnectionRequestPassword is not null after reboot";
	    result = CommonMethods.isNull(tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_CONNECTIONREQUESTPASSWORD));
	    if (result) {
		LOGGER.info(
			"STEP 15: ACTUAL : Null value of Device ManagementServer ConnectionRequestPassword persists after reboot");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 16: Execute any TR69 query.
	     */
	    step = "s16";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION :Execute any TR69 query.");
	    LOGGER.info("STEP 16: ACTION : Verify Tr69 query execution should not successfully");
	    LOGGER.info(
		    "STEP 16: EXPECTED :  TR69 query failed with connection error due to changed password persisting after reboot");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "TR69 query did not fail with connection error due to changed password persisting after reboot";
	    try {
		get_param = BroadBandTr69Utils.getParameterForTr69Get(BroadBandWebPaConstants.TR69_PARAM_SERIAL_NUMBER);
		response = tapEnv.getTR69ParameterValues(device, get_param);
	    } catch (Exception exception) {
		response = exception.getMessage();
	    }
	    LOGGER.info("Response obtained from TR69 query: " + response);
	    result = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.STRING_FAILED);
	    if (result) {
		LOGGER.info(
			"STEP 16: ACTUAL : TR69 query failed with connection error due to changed password persisting after reboot");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	    /*
	     * Step 17: Reset default value of Device ManagementServer Password
	     */
	    step = "s17";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION :Reset default value of Device ManagementServer Password");
	    LOGGER.info("STEP 17: ACTION : Reset default value of Device ManagementServer Password using webpa "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_PASSWORD);
	    LOGGER.info("STEP 17: EXPECTED :  Successfully reset default value of Device ManagementServer Password");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to reset default value of Device ManagementServer Password";
	    result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_PASSWORD,
		    BroadBandTestConstants.CONSTANT_0, defaultMgmtPwd);
	    if (result) {
		LOGGER.info("STEP 17: ACTUAL : Successfully reset default value of Device ManagementServer Password");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, true);

	    /*
	     * Step 18: Execute any TR69 query.
	     */
	    step = "s18";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION :Execute any TR69 query.");
	    LOGGER.info("STEP 18: ACTION : Verify Tr69 query executed successfully");
	    LOGGER.info("STEP 18: EXPECTED :   TR69 query works as expected");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "TR69 query failed to execute successfully after password reset";
	    startTime = System.currentTimeMillis();
	    while (System.currentTimeMillis() - startTime < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS && !result
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
		response = null;
		try {
		    get_param = BroadBandTr69Utils
			    .getParameterForTr69Get(BroadBandWebPaConstants.TR69_PARAM_SERIAL_NUMBER);
		    response = tapEnv.getTR69ParameterValues(device, get_param);
		} catch (Exception exception) {
		    errorMessage = exception.getMessage();
		}
		LOGGER.info("Expected serial number: " + device.getSerialNumber());
		LOGGER.info("Serial number obtained from TR69 query: " + response);
		result = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(device.getSerialNumber());
	    }
	    if (result) {
		LOGGER.info("STEP 18: ACTUAL : TR69 query works as expected");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, result, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating TR69 passwords functionality: " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, result, errorMessage, true);

	} finally {
	    BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    if (CommonMethods.isNotNull(defaultMgmtPwd)) {
		LOGGER.info("****************************************************************************************");
		LOGGER.info("Post conditions:");
		LOGGER.info("1. Reset default value of Device ManagementServer Password");
		LOGGER.info("2. Reactivate device after factory reset");
		LOGGER.info("****************************************************************************************");

		BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MANAGEMENTSERVER_PASSWORD,
			BroadBandTestConstants.CONSTANT_0, defaultMgmtPwd, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
			BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
	    }
	    BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
	    LOGGER.info("ENDING TEST CASE : TC-RDKB-TR69-1005");
	}
    }

    /*
     * Test to validate device IP objects against TR69 specifications
     * 
     * Step1: Verification of the object Device.IP.IPv4Capable via TR69 Step2: Validation of the Object
     * Device.IP.IPv4Capable via WebPa Step3: Verification of the object Device.IP.IPv4Enable via TR69 Step4: Validation
     * of the Object Device.IP.IPv4Enable via WebPa Step5: Verification of the object Device.IP.IPv4Status via TR69
     * Step6: Validation of the Object Device.IP.IPv4Status via WebPa Step7: Verification of the object
     * Device.IP.IPv6Capable via TR69 Step8: Validation of the Object Device.IP.IPv6Capable via WebPa Step9:
     * Verification of the object Device.IP.IPv6Enable via TR69 Step10: Validation of the Object Device.IP.IPv6Enable
     * via WebPa Step11: Verification of the object Device.IP.IPv6Status via TR69 Step12: Validation of the Object
     * Device.IP.IPv6Status via WebPa
     *
     * @author Sathurya Ravi
     * 
     * @refactor Rakesh C N
     * 
     * @param device to be used for execution
     *
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1020")
    public void testValidateTR69IPInterfaceObjects(Dut device) {

	// stores the test result
	boolean status = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-020";
	// stores the error message
	String message = null;
	// stores the stepNumber
	String stepNumber = null;
	// stores the command response
	String response = null;
	// stores parameterList
	String parameterList = null;
	// stores response from previous step
	String responseFromPreviousStep = "";

	List<String> get_param = new ArrayList<String>();

	try {

	    /*
	     * stepNumber 1: Verification of the object Device.IP.IPv4Capable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 1: DESCRIPTION: Verification of the object Device.IP.IPv4Capable via TR69");
	    LOGGER.info(
		    "stepNumber 1: ACTION: a) Execute getParameterName on the object Device.IP.IPv4Capable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info("stepNumber 1: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s1";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV4CAPABLE);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(AutomaticsConstants.TRUE));
		message = "The response for the parameter Device.IP.IPv4Capable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.IPv4Capable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 2: Validation of the Object Device.IP.IPv4Capable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 2: DESCRIPTION: Validation of the Object Device.IP.IPv4Capable via WebPa ");
	    LOGGER.info(
		    "stepNumber 2: ACTION: a) Execute WebPa GET command on the object Device.IP.IPv4Capable using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.IPv4Capable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 2: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s2";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV4CAPABLE);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);

	    message = "The Webpa Response for the parameter Device.IP.IPv4Capable: " + response + " Expected: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 3: Verification of the object Device.IP.IPv4Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 3: DESCRIPTION: Verification of the object Device.IP.IPv4Enable via TR69 ");
	    LOGGER.info(
		    "stepNumber 3: ACTION: a) Execute getParameterName on the object Device.IP.IPv4Capable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info("stepNumber 3: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s3";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV4ENABLE);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(AutomaticsConstants.TRUE));
		message = "The response for the parameter Device.IP.IPv4Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.IPv4Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 4: Validation of the Object Device.IP.IPv4Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 4: DESCRIPTION: Validation of the Object Device.IP.IPv4Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 4: ACTION: a) Execute WebPa GET command on the object Device.IP.IPv4Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.IPv4Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 4: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s4";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV4ENABLE);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response : " + response + " TR69: " + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 5: Verification of the object Device.IP.IPv4Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 5: DESCRIPTION: Verification of the object Device.IP.IPv4Status via TR69 ");
	    LOGGER.info(
		    "stepNumber 5: ACTION: a) Execute getParameterName on the object Device.IP.IPv4Status  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate the type and value of the parameter.");
	    LOGGER.info("stepNumber 5: EXPECTED: The value returned should be a string and it should be \"Enabled\".");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s5";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV4STATUS);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equals(BroadBandTestConstants.INTERFACE_ENABLED_STATUS));
		message = "The response for the parameter Device.IP.IPv4Status : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.IPv4Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 6: Validation of the Object Device.IP.IPv4Status via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 6: DESCRIPTION: Validation of the Object Device.IP.IPv4Status via WebPa ");
	    LOGGER.info(
		    "stepNumber 6: ACTION: a) Execute WebPa GET command on the object Device.IP.IPv4Status using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.IPv4Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 6: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s6";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV4STATUS);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 7: Verification of the object Device.IP.IPv6Capable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 7: DESCRIPTION: Verification of the object Device.IP.IPv6Capable via TR69");
	    LOGGER.info(
		    "stepNumber 7: ACTION: a) Execute getParameterName on the object Device.IP.IPv4Capable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info("stepNumber 7: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s7";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV6CAPABLE);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(AutomaticsConstants.TRUE));
		message = "The response for the parameter Device.IP.IPv6Capable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.IPv6Capable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 8: Validation of the Object Device.IP.IPv6Capable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 8: DESCRIPTION: Validation of the Object Device.IP.IPv6Capable via WebPa ");
	    LOGGER.info(
		    "stepNumber 8: ACTION: a) Execute WebPa GET command on the object Device.IP.IPv6Capable using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.IPv6Capable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 8: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s8";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV6CAPABLE);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 9: Verification of the object Device.IP.IPv6Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 9: DESCRIPTION: Verification of the object Device.IP.IPv6Enable via TR69 ");
	    LOGGER.info(
		    "stepNumber 9: ACTION: a) Execute getParameterName on the object Device.IP.IPv6Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info("stepNumber 9: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s9";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV6ENABLE);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(AutomaticsConstants.TRUE));
		message = "The response for the parameter Device.IP.IPv6Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.IPv6Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 10: Validation of the Object Device.IP.IPv6Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 10: DESCRIPTION: Validation of the Object Device.IP.IPv6Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 10: ACTION: a) Execute WebPa GET command on the object Device.IP.IPv6Enable using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.IPv6Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 10: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s10";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV6ENABLE);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 11: Verification of the object Device.IP.IPv6Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 11: DESCRIPTION: Verification of the object Device.IP.IPv6Status via TR69 ");
	    LOGGER.info(
		    "stepNumber 11: ACTION: a) Execute getParameterName on the object Device.IP.IPv6Status  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate the type and value of the parameter.");
	    LOGGER.info("stepNumber 11: EXPECTED: The value returned should be a string and it should be \"Enabled\".");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s11";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV6STATUS);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equals(BroadBandTestConstants.INTERFACE_ENABLED_STATUS));
		message = "The response for the parameter Device.IP.IPv6Status : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.IPv6Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 12: Validation of the Object Device.IP.IPv6Status via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 12: DESCRIPTION: Validation of the Object Device.IP.IPv6Status via WebPa ");
	    LOGGER.info(
		    "stepNumber 12: ACTION: a) Execute WebPa GET command on the object Device.IP.IPv6Status using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.IPv6Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 12: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s12";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_IP_IPV6STATUS);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	} catch (Exception exception) {
	    message = exception.getMessage();
	    LOGGER.error("Exception occured while validating TR69 IP interface objects " + message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, message, true);
	}
    }

	   /*
	    * Test to validate Device interface objects as per TR69 specifications
	    * 
		* Step1: Verification of the object Device.IP.Interface.1.Enable via TR69
		* Step2: Validation of the Object Device.IP.Interface.1.Enable via WebPa
		* Step3: Verification of the object Device.IP.Interface.1.IPv4Enable via TR69
		* Step4: Validate the TR69 response of the object Device.IP.Interface.1.IPv4Enable against WebPa response
		* Step5: Verification of the object Device.IP.Interface.1.IPv6Enable via TR69
		* Step6: Validation of the Object Device.IP.Interface.1.IPv6Enable via WebPa
		* Step7: Verification of the object Device.IP.Interface.1.LastChange via TR69
		* Step8: Validation of the Object Device.IP.Interface.1.LastChange via WebPa
		* Step9: Verification of the object Device.IP.Interface.1.MaxMTUSize via TR69
		* Step10: Validation of the Object Device.IP.Interface.1.MaxMTUSize via WebPa
		* Step11: Verification of the object Device.IP.Interface.1.Status via TR69
		* Step12: Validation of the Object Device.IP.Interface.1.Status via WebPa
		* Step13: Verification of the object Device.IP.Interface.2.Enable via TR69
		* Step14: Validation of the Object Device.IP.Interface.2.Enable via WebPa
		* Step15: Verification of the object Device.IP.Interface.2.IPv4Enable via TR69
		* Step16: Validate the TR69 response of the object Device.IP.Interface.2.IPv4Enable against WebPa response
		* Step17: Verification of the object Device.IP.Interface.2.IPv6Enable via TR69
		* Step18: Validation of the Object Device.IP.Interface.2.IPv6Enable via WebPa
		* Step19: Verification of the object Device.IP.Interface.2.LastChange via TR69
		* Step20: Validation of the Object Device.IP.Interface.2.LastChange via WebPa
		* Step21: Verification of the object Device.IP.Interface.2.MaxMTUSize via TR69
		* Step22: Validation of the Object Device.IP.Interface.2.MaxMTUSize via WebPa
		* Step23: Verification of the object Device.IP.Interface.2.Status via TR69
		* Step24: Validation of the Object Device.IP.Interface.2.Status via WebPa
		* Step25: Verification of the object Device.IP.Interface.3.Enable via TR69
		* Step26: Validation of the Object Device.IP.Interface.3.Enable via WebPa
		* Step27: Verification of the object Device.IP.Interface.3.IPv4Enable via TR69
		* Step28: Validate the TR69 response of the object Device.IP.Interface.3.IPv4Enable against WebPa response
		* Step29: Verification of the object Device.IP.Interface.3.IPv6Enable via TR69
		* Step30: Validation of the Object Device.IP.Interface.3.IPv6Enable via WebPa
		* Step31: Verification of the object Device.IP.Interface.3.LastChange via TR69
		* Step32: Validation of the Object Device.IP.Interface.3.LastChange via WebPa
		* Step33: Verification of the object Device.IP.Interface.3.MaxMTUSize via TR69
		* Step34: Validation of the Object Device.IP.Interface.3.MaxMTUSize via WebPa
		* Step35: Verification of the object Device.IP.Interface.3.Status via TR69
		* Step36: Validation of the Object Device.IP.Interface.3.Status via WebPa
		* Step37: Verification of the object Device.IP.Interface.4.Enable via TR69
		* Step38: Validation of the Object Device.IP.Interface.4.Enable via WebPa
		* Step39: Verification of the objectDevice.IP.Interface.4.IPv4Enable via TR69
		* Step40: Validate the TR69 response of the object Device.IP.Interface.4.IPv4Enable against WebPa response
		* Step41: Verification of the object Device.IP.Interface.4.IPv6Enable via TR69
		* Step42: Validation of the Object Device.IP.Interface.4.IPv6Enable via WebPa
		* Step43: Verification of the object Device.IP.Interface.4.LastChange via TR69
		* Step44: Validation of the Object Device.IP.Interface.4.LastChange via WebPa
		* Step45: Verification of the object Device.IP.Interface.4.MaxMTUSize via TR69
		* Step46: Validation of the Object Device.IP.Interface.4.MaxMTUSize via WebPa
		* Step47: Verification of the object Device.IP.Interface.4.Status via TR69
		* Step48: Validation of the Object Device.IP.Interface.4.Status via WebPa
		* Step49: Verification of the object Device.IP.Interface.5.Enable via TR69
		* Step50: Validation of the Object Device.IP.Interface.5.Enable via WebPa
		* Step51: Verification of the object Device.IP.Interface.5.IPv4Enable via TR69
		* Step52: Validate the TR69 response of the object Device.IP.Interface.5.IPv4Enable against WebPa response
		* Step53: Verification of the object Device.IP.Interface.5.IPv6Enable via TR69
		* Step54: Validation of the Object Device.IP.Interface.5.IPv6Enable via WebPa
		* Step55: Verification of the object Device.IP.Interface.5.LastChange via TR69
		* Step56: Validation of the Object Device.IP.Interface.5.LastChange via WebPa
		* Step57: Verification of the object Device.IP.Interface.5.MaxMTUSize via TR69
		* Step58: Validation of the Object Device.IP.Interface.5.MaxMTUSize via WebPa
		* Step59: Verification of the object Device.IP.Interface.5.Status via TR69
		* Step60: Validation of the Object Device.IP.Interface.5.Status via WebPa
		* Step61: Verification of the object Device.IP.Interface.6.Enable via TR69
		* Step62: Validation of the Object Device.IP.Interface.6.Enable via WebPa
		* Step63: Verification of the object Device.IP.Interface.6.IPv4Enable via TR69
		* Step64: Validate the TR69 response of the object Device.IP.Interface.6.IPv4Enable against WebPa response
		* Step65: Verification of the object Device.IP.Interface.6.IPv6Enable via TR69
		* Step66: Validation of the Object Device.IP.Interface.6.IPv6Enable via WebPa
		* Step67: Verification of the object Device.IP.Interface.6.LastChange via TR69
		* Step68: Validation of the Object Device.IP.Interface.6.LastChange via WebPa
		* Step69: Verification of the object Device.IP.Interface.6.MaxMTUSize via TR69
		* Step70: Validation of the Object Device.IP.Interface.6.MaxMTUSize viaWebPa
		* Step71: Verification of the object Device.IP.Interface.6.Status via TR69
		* Step72: Validation of the Object Device.IP.Interface.6.Status via WebPa
	    *
	    * @author Sathurya Ravi
	    * 
	    * @refactor Rakesh C N
	    * 
	    * @param device to be used for execution
	    *
	    */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1021")

    public void testValidateTR69InterfaceObjects(Dut device) {

	// stores the test result
	boolean status = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-021";
	// stores the error message
	String message = null;
	// stores the stepNumber
	String stepNumber = null;
	// stores the command response
	String response = null;
	// stores the List of parameter returned by the TR69 response.
	List<Parameter> parameterTr69Response = null;
	// stores previous step response
	String responseFromPreviousStep = null;
	// stores parameterList
	List<Parameter> parameterList = null;

	List<String> get_param = new ArrayList<String>();

	try {

	    /*
	     * stepNumber 1: Verification of the object Device.IP.Interface.1.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 1: DESCRIPTION: Verification of the object Device.IP.Interface.1.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 1: ACTION: a) Execute getParameterName on the object Device.IP.Interface.1.Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info("stepNumber 1: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s1";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.1.Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.1.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 2: Validation of the Object Device.IP.Interface.1.Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 2: DESCRIPTION: Validation of the Object Device.IP.Interface.1.Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 2: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.1.Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.1.Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 2: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s2";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 3: Verification of the object Device.IP.Interface.1.IPv4Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 3: DESCRIPTION: Verification of the object Device.IP.Interface.1.IPv4Enable via TR69");
	    LOGGER.info(
		    "stepNumber 3: ACTION: a) Execute getParameterName on the object Device.IP.Interface.1.IPv4Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info("stepNumber 3: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s3";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.1.IPv4Enable: " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.1.IPv4Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 4: Validation of the Object Device.IP.Interface.1.IPv4Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 4: DESCRIPTION: Validation of the Object Device.IP.Interface.1.IPv4Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 4: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.1.IPv4Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.1.IPv4Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 4: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s4";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 5: Verification of the object Device.IP.Interface.1.IPv6Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 5: DESCRIPTION: Verification of the object Device.IP.Interface.1.IPv6Enable via TR69");
	    LOGGER.info(
		    "stepNumber 5: ACTION: a) Execute getParameterName on the object Device.IP.Interface.1.IPv6Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info("stepNumber 5: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s5";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.1.IPv6Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.1.IPv6Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 6: Validation of the Object Device.IP.Interface.1.IPv6Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 6: DESCRIPTION: Validation of the Object Device.IP.Interface.1.IPv6Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 6: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.1.IPv6Enable using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.1.IPv6Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 6: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s6";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 7: Verification of the object Device.IP.Interface.1.LastChange via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 7: DESCRIPTION: Verification of the object Device.IP.Interface.1.LastChange via TR69 ");
	    LOGGER.info(
		    "stepNumber 7: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.1.LastChange using the TR69 Rest API ,"
			    + "\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.1.LastChange");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 7: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s7";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.LASTCHANGE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {

		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.IP.Interface.1.LastChange is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		message = "The response for the parameter Device.IP.Interface.6.Stats.UnknownProtoPacketsReceived: "
			+ response + " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.Radio.10000.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 8: Verification of the object Device.IP.Interface.1.MaxMTUSize via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 8: DESCRIPTION: Verification of the object Device.IP.Interface.1.MaxMTUSize via TR69 ");
	    LOGGER.info(
		    "stepNumber 8: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.1.MaxMTUSize using the TR69 Rest API "
			    + ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.1.MaxMTUSize");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 8: EXPECTED: The value returned should be a valid unsigned integer and the value be between 64 and 65536 ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s8";
	    status = false;
	    get_param.clear();

	    try {

		get_param = BroadBandTr69Utils.getParameterForTr69Get(
			TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));
		response = tapEnv.getTR69ParameterValues(device, get_param);
		LOGGER.info("Result of get operation is:" + response);

		if (response != null) {
		    status = CommonMethods.isNotNull(response)
			    && (Integer.parseInt(response) >= 64 && Integer.parseInt(response) <= 65536);
		    responseFromPreviousStep = response;
		    message = "The response for the parameter Device.IP.Interface.1.MaxMTUSize: " + response
			    + " |Expected value should be a valid unsigned integer.";

		} else {
		    message = "No response is returned when the TR69 parameter Device.IP.Interface.1.MaxMTUSize is queried.";
		}

	    } catch (NumberFormatException e) {

		LOGGER.error(
			"The value returned as response for parameter Device.IP.Interface.1.MaxMTUSize is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			e);
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 9: Validation of the Object Device.IP.Interface.1.MaxMTUSize via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 9: DESCRIPTION: Validation of the Object Device.IP.Interface.1.MaxMTUSize via WebPa ");
	    LOGGER.info(
		    "stepNumber 9: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.1.MaxMTUSize using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.1.MaxMTUSize' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 9: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s9";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 10: Verification of the object Device.IP.Interface.1.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 10: DESCRIPTION: Verification of the object Device.IP.Interface.1.Status via TR69");
	    LOGGER.info(
		    "stepNumber 10: ACTION: a) Execute getParameterName on the object Device.IP.Interface.1.Status  using the TR69 Rest API");
	    LOGGER.info("                       b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 10: EXPECTED: The value returned should be of type String and the value should be Up. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s10";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equalsIgnoreCase(BroadBandTestConstants.STRING_UP));
		responseFromPreviousStep = response;
		message = "The response for the parameter Device.IP.Interface.1.Status : " + response
			+ "| Expected should be Up";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.1.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 11: Validation of the Object Device.IP.Interface.1.Status via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 11: DESCRIPTION: Validation of the Object Device.IP.Interface.1.Status via WebPa ");
	    LOGGER.info(
		    "stepNumber 11: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.1.Status using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.1.Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 11: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s11";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
	    		TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_1));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 12: Verification of the object Device.IP.Interface.2.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 12: DESCRIPTION: Verification of the object Device.IP.Interface.2.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 12: ACTION: a) Execute getParameterName on the object Device.IP.Interface.2.Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 12: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s12";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.2.Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.2.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 13: Validation of the Object Device.IP.Interface.2.Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 13: DESCRIPTION: Validation of the Object Device.IP.Interface.2.Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 13: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.2.Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.2.Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 13: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s13";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 14: Verification of the object Device.IP.Interface.2.IPv4Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 14: DESCRIPTION: Verification of the object Device.IP.Interface.2.IPv4Enable via TR69");
	    LOGGER.info(
		    "stepNumber 14: ACTION: a) Execute getParameterName on the object Device.IP.Interface.2.IPv4Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 14: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s14";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.2.IPv4Enable: " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.2.IPv4Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 15: Validation of the Object Device.IP.Interface.2.IPv4Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 15: DESCRIPTION: Validation of the Object Device.IP.Interface.2.IPv4Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 15: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.2.IPv4Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.2.IPv4Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 15: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s15";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 16: Verification of the object Device.IP.Interface.2.IPv6Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 16: DESCRIPTION: Verification of the object Device.IP.Interface.2.IPv6Enable via TR69");
	    LOGGER.info(
		    "stepNumber 16: ACTION: a) Execute getParameterName on the object Device.IP.Interface.2.IPv6Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 16: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s16";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.2.IPv6Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.2.IPv6Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 17: Validation of the Object Device.IP.Interface.2.IPv6Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 17: DESCRIPTION: Validation of the Object Device.IP.Interface.2.IPv6Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 17: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.2.IPv6Enable using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.2.IPv6Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 17: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s17";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 18: Verification of the object Device.IP.Interface.2.LastChange via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 18: DESCRIPTION: Verification of the object Device.IP.Interface.2.LastChange via TR69 ");
	    LOGGER.info(
		    "stepNumber 18: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.2.LastChange using the TR69 Rest API ,"
			    + "\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.2.LastChange");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 18: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s18";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.LASTCHANGE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {

		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.IP.Interface.2.LastChange is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		message = "The response for the parameter Device.IP.Interface.6.Stats.UnknownProtoPacketsReceived: "
			+ response + " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.Radio.10000.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 19: Verification of the object Device.IP.Interface.2.MaxMTUSize via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 19: DESCRIPTION: Verification of the object Device.IP.Interface.2.MaxMTUSize via TR69 ");
	    LOGGER.info(
		    "stepNumber 19: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.2.MaxMTUSize using the TR69 Rest API "
			    + ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.2.MaxMTUSize");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 19: EXPECTED: The value returned should be a valid unsigned integer and the value be between 64 and 65536 ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s19";
	    status = false;
	    get_param.clear();

	    try {

		get_param = BroadBandTr69Utils.getParameterForTr69Get(
			TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));
		response = tapEnv.getTR69ParameterValues(device, get_param);
		LOGGER.info("Result of get operation is:" + response);

		if (response != null) {
		    status = CommonMethods.isNotNull(response)
			    && (Integer.parseInt(response) >= 64 && Integer.parseInt(response) <= 65536);
		    responseFromPreviousStep = response;
		    message = "The response for the parameter Device.IP.Interface.2.MaxMTUSize: " + response
			    + " |Expected value should be a valid unsigned integer.";

		} else {
		    message = "No response is returned when the TR69 parameter Device.IP.Interface.2.MaxMTUSize is queried.";
		}

	    } catch (NumberFormatException e) {

		LOGGER.error(
			"The value returned as response for parameter Device.IP.Interface.2.MaxMTUSize is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			e);
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 20: Validation of the Object Device.IP.Interface.2.MaxMTUSize via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 20: DESCRIPTION: Validation of the Object Device.IP.Interface.2.MaxMTUSize via WebPa ");
	    LOGGER.info(
		    "stepNumber 20: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.2.MaxMTUSize using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.2.MaxMTUSize' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 20: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s20";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 21: Verification of the object Device.IP.Interface.2.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 21: DESCRIPTION: Verification of the object Device.IP.Interface.2.Status via TR69");
	    LOGGER.info(
		    "stepNumber 21: ACTION: a) Execute getParameterName on the object Device.IP.Interface.2.Status  using the TR69 Rest API");
	    LOGGER.info("                       b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 21: EXPECTED: The value returned should be of type String and the value should be Up. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s21";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equalsIgnoreCase(BroadBandTestConstants.STRING_UP));
		responseFromPreviousStep = response;
		message = "The response for the parameter Device.IP.Interface.2.Status : " + response
			+ "| Expected should be Up";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.2.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 22: Validation of the Object Device.IP.Interface.2.Status via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 22: DESCRIPTION: Validation of the Object Device.IP.Interface.2.Status via WebPa ");
	    LOGGER.info(
		    "stepNumber 22: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.2.Status using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.2.Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 22: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s22";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_2));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 23: Verification of the object Device.IP.Interface.3.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 23: DESCRIPTION: Verification of the object Device.IP.Interface.3.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 23: ACTION: a) Execute getParameterName on the object Device.IP.Interface.3.Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 23: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s23";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.3.Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.3.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 24: Validation of the Object Device.IP.Interface.3.Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 24: DESCRIPTION: Validation of the Object Device.IP.Interface.3.Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 24: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.3.Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.3.Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 24: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s24";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 25: Verification of the object Device.IP.Interface.3.IPv4Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 25: DESCRIPTION: Verification of the object Device.IP.Interface.3.IPv4Enable via TR69");
	    LOGGER.info(
		    "stepNumber 25: ACTION: a) Execute getParameterName on the object Device.IP.Interface.3.IPv4Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 25: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s25";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.3.IPv4Enable: " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.3.IPv4Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 26: Validation of the Object Device.IP.Interface.3.IPv4Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 26: DESCRIPTION: Validation of the Object Device.IP.Interface.3.IPv4Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 26: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.3.IPv4Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.3.IPv4Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 26: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s26";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 27: Verification of the object Device.IP.Interface.3.IPv6Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 27: DESCRIPTION: Verification of the object Device.IP.Interface.3.IPv6Enable via TR69");
	    LOGGER.info(
		    "stepNumber 27: ACTION: a) Execute getParameterName on the object Device.IP.Interface.3.IPv6Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 27: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s27";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.3.IPv6Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.3.IPv6Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 28: Validation of the Object Device.IP.Interface.3.IPv6Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 28: DESCRIPTION: Validation of the Object Device.IP.Interface.3.IPv6Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 28: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.3.IPv6Enable using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.3.IPv6Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 28: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s28";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 29: Verification of the object Device.IP.Interface.3.LastChange via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 29: DESCRIPTION: Verification of the object Device.IP.Interface.3.LastChange via TR69 ");
	    LOGGER.info(
		    "stepNumber 29: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.3.LastChange using the TR69 Rest API ,"
			    + "\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.3.LastChange");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 29: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s29";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.LASTCHANGE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {

		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.IP.Interface.3.LastChange is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		message = "The response for the parameter Device.IP.Interface.6.Stats.UnknownProtoPacketsReceived: "
			+ response + " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.Radio.10000.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 30: Verification of the object Device.IP.Interface.3.MaxMTUSize via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 30: DESCRIPTION: Verification of the object Device.IP.Interface.3.MaxMTUSize via TR69 ");
	    LOGGER.info(
		    "stepNumber 30: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.3.MaxMTUSize using the TR69 Rest API "
			    + ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.3.MaxMTUSize");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 30: EXPECTED: The value returned should be a valid unsigned integer and the value be between 64 and 65536 ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s30";
	    status = false;
	    get_param.clear();

	    try {

		get_param = BroadBandTr69Utils.getParameterForTr69Get(
			TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));
		response = tapEnv.getTR69ParameterValues(device, get_param);
		LOGGER.info("Result of get operation is:" + response);

		if (response != null) {
		    status = CommonMethods.isNotNull(response)
			    && (Integer.parseInt(response) >= 64 && Integer.parseInt(response) <= 65536);
		    responseFromPreviousStep = response;
		    message = "The response for the parameter Device.IP.Interface.3.MaxMTUSize: " + response
			    + " |Expected value should be a valid unsigned integer.";

		} else {
		    message = "No response is returned when the TR69 parameter Device.IP.Interface.3.MaxMTUSize is queried.";
		}

	    } catch (NumberFormatException e) {

		LOGGER.error(
			"The value returned as response for parameter Device.IP.Interface.3.MaxMTUSize is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			e);
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 31: Validation of the Object Device.IP.Interface.3.MaxMTUSize via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 31: DESCRIPTION: Validation of the Object Device.IP.Interface.3.MaxMTUSize via WebPa ");
	    LOGGER.info(
		    "stepNumber 31: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.3.MaxMTUSize using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.3.MaxMTUSize' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 31: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s31";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 32: Verification of the object Device.IP.Interface.3.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 32: DESCRIPTION: Verification of the object Device.IP.Interface.3.Status via TR69");
	    LOGGER.info(
		    "stepNumber 32: ACTION: a) Execute getParameterName on the object Device.IP.Interface.3.Status  using the TR69 Rest API");
	    LOGGER.info("                       b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 32: EXPECTED: The value returned should be of type String and the value should be Up. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s32";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equalsIgnoreCase(BroadBandTestConstants.STRING_UP));
		responseFromPreviousStep = response;
		message = "The response for the parameter Device.IP.Interface.3.Status : " + response
			+ "| Expected should be Up";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.3.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 33: Validation of the Object Device.IP.Interface.3.Status via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 33: DESCRIPTION: Validation of the Object Device.IP.Interface.3.Status via WebPa ");
	    LOGGER.info(
		    "stepNumber 33: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.3.Status using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.3.Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 33: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s33";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_3));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 34: Verification of the object Device.IP.Interface.4.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 34: DESCRIPTION: Verification of the object Device.IP.Interface.4.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 34: ACTION: a) Execute getParameterName on the object Device.IP.Interface.4.Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 34: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s34";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));
	    response = tapEnv.getTR69ParameterValues(device, get_param);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.4.Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.4.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 35: Validation of the Object Device.IP.Interface.4.Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 35: DESCRIPTION: Validation of the Object Device.IP.Interface.4.Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 35: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.4.Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.4.Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 35: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s35";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 36: Verification of the object Device.IP.Interface.4.IPv4Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 36: DESCRIPTION: Verification of the object Device.IP.Interface.4.IPv4Enable via TR69");
	    LOGGER.info(
		    "stepNumber 36: ACTION: a) Execute getParameterName on the object Device.IP.Interface.4.IPv4Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 36: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s36";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.4.IPv4Enable: " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.4.IPv4Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 37: Validation of the Object Device.IP.Interface.4.IPv4Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 37: DESCRIPTION: Validation of the Object Device.IP.Interface.4.IPv4Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 37: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.4.IPv4Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.4.IPv4Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 37: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s37";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 38: Verification of the object Device.IP.Interface.4.IPv6Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 38: DESCRIPTION: Verification of the object Device.IP.Interface.4.IPv6Enable via TR69");
	    LOGGER.info(
		    "stepNumber 38: ACTION: a) Execute getParameterName on the object Device.IP.Interface.4.IPv6Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 38: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s38";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.4.IPv6Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.4.IPv6Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 39: Validation of the Object Device.IP.Interface.4.IPv6Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 39: DESCRIPTION: Validation of the Object Device.IP.Interface.4.IPv6Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 39: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.4.IPv6Enable using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.4.IPv6Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 39: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s39";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 40: Verification of the object Device.IP.Interface.4.LastChange via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 40: DESCRIPTION: Verification of the object Device.IP.Interface.4.LastChange via TR69 ");
	    LOGGER.info(
		    "stepNumber 40: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.4.LastChange using the TR69 Rest API ,"
			    + "\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.4.LastChange");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 40: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s40";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.LASTCHANGE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {

		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.IP.Interface.4.LastChange is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		message = "The response for the parameter Device.IP.Interface.6.Stats.UnknownProtoPacketsReceived: "
			+ response + " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.Radio.10000.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 41: Verification of the object Device.IP.Interface.4.MaxMTUSize via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 41: DESCRIPTION: Verification of the object Device.IP.Interface.4.MaxMTUSize via TR69 ");
	    LOGGER.info(
		    "stepNumber 41: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.4.MaxMTUSize using the TR69 Rest API "
			    + ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.4.MaxMTUSize");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 41: EXPECTED: The value returned should be a valid unsigned integer and the value be between 64 and 65536 ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s41";
	    status = false;
	    get_param.clear();

	    try {

		get_param = BroadBandTr69Utils.getParameterForTr69Get(
			TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));
		response = tapEnv.getTR69ParameterValues(device, get_param);
		LOGGER.info("Result of get operation is:" + response);

		if (response != null) {
		    status = CommonMethods.isNotNull(response)
			    && (Integer.parseInt(response) >= 64 && Integer.parseInt(response) <= 65536);
		    responseFromPreviousStep = response;
		    message = "The response for the parameter Device.IP.Interface.4.MaxMTUSize: " + response
			    + " |Expected value should be a valid unsigned integer.";

		} else {
		    message = "No response is returned when the TR69 parameter Device.IP.Interface.4.MaxMTUSize is queried.";
		}

	    } catch (NumberFormatException e) {

		LOGGER.error(
			"The value returned as response for parameter Device.IP.Interface.4.MaxMTUSize is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			e);
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 42: Validation of the Object Device.IP.Interface.4.MaxMTUSize via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 42: DESCRIPTION: Validation of the Object Device.IP.Interface.4.MaxMTUSize via WebPa ");
	    LOGGER.info(
		    "stepNumber 42: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.4.MaxMTUSize using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.4.MaxMTUSize' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 42: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s42";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 43: Verification of the object Device.IP.Interface.4.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 43: DESCRIPTION: Verification of the object Device.IP.Interface.4.Status via TR69");
	    LOGGER.info(
		    "stepNumber 43: ACTION: a) Execute getParameterName on the object Device.IP.Interface.4.Status  using the TR69 Rest API");
	    LOGGER.info("                       b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 43: EXPECTED: The value returned should be of type String and the value should be Up. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s43";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_UP);
		responseFromPreviousStep = response;
		message = "The response for the parameter Device.IP.Interface.4.Status : " + response
			+ "| Expected should be Up";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.4.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 44: Validation of the Object Device.IP.Interface.4.Status via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 44: DESCRIPTION: Validation of the Object Device.IP.Interface.4.Status via WebPa ");
	    LOGGER.info(
		    "stepNumber 44: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.4.Status using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.4.Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 44: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s44";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_4));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 45: Verification of the object Device.IP.Interface.5.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 45: DESCRIPTION: Verification of the object Device.IP.Interface.5.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 45: ACTION: a) Execute getParameterName on the object Device.IP.Interface.5.Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 45: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s45";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.5.Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.5.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 46: Validation of the Object Device.IP.Interface.5.Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 46: DESCRIPTION: Validation of the Object Device.IP.Interface.5.Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 46: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.5.Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.5.Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 46: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s46";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 47: Verification of the object Device.IP.Interface.5.IPv4Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 47: DESCRIPTION: Verification of the object Device.IP.Interface.5.IPv4Enable via TR69");
	    LOGGER.info(
		    "stepNumber 47: ACTION: a) Execute getParameterName on the object Device.IP.Interface.5.IPv4Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 47: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s47";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.5.IPv4Enable: " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.5.IPv4Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 48: Validation of the Object Device.IP.Interface.5.IPv4Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 48: DESCRIPTION: Validation of the Object Device.IP.Interface.5.IPv4Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 48: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.5.IPv4Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.5.IPv4Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 48: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s48";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 49: Verification of the object Device.IP.Interface.5.IPv6Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 49: DESCRIPTION: Verification of the object Device.IP.Interface.5.IPv6Enable via TR69");
	    LOGGER.info(
		    "stepNumber 49: ACTION: a) Execute getParameterName on the object Device.IP.Interface.5.IPv6Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 49: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s49";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.5.IPv6Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.5.IPv6Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 50: Validation of the Object Device.IP.Interface.5.IPv6Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 50: DESCRIPTION: Validation of the Object Device.IP.Interface.5.IPv6Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 50: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.5.IPv6Enable using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.5.IPv6Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 50: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s50";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 51: Verification of the object Device.IP.Interface.5.LastChange via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 51: DESCRIPTION: Verification of the object Device.IP.Interface.5.LastChange via TR69 ");
	    LOGGER.info(
		    "stepNumber 51: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.5.LastChange using the TR69 Rest API ,"
			    + "\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.5.LastChange");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 51: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s51";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.LASTCHANGE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {

		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.IP.Interface.5.LastChange is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		message = "The response for the parameter Device.IP.Interface.6.Stats.UnknownProtoPacketsReceived: "
			+ response + " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.Radio.10000.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 52: Verification of the object Device.IP.Interface.5.MaxMTUSize via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 52: DESCRIPTION: Verification of the object Device.IP.Interface.5.MaxMTUSize via TR69 ");
	    LOGGER.info(
		    "stepNumber 52: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.5.MaxMTUSize using the TR69 Rest API "
			    + ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.5.MaxMTUSize");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 52: EXPECTED: The value returned should be a valid unsigned integer and the value be between 64 and 65547 ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s52";
	    status = false;
	    get_param.clear();

	    try {

		get_param = BroadBandTr69Utils.getParameterForTr69Get(
			TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));
		response = tapEnv.getTR69ParameterValues(device, get_param);
		LOGGER.info("Result of get operation is:" + response);

		if (response != null) {
		    status = CommonMethods.isNotNull(response)
			    && (Integer.parseInt(response) >= 64 && Integer.parseInt(response) <= 65547);
		    responseFromPreviousStep = response;
		    message = "The response for the parameter Device.IP.Interface.5.MaxMTUSize: " + response
			    + " |Expected value should be a valid unsigned integer.";

		} else {
		    message = "No response is returned when the TR69 parameter Device.IP.Interface.5.MaxMTUSize is queried.";
		}

	    } catch (NumberFormatException e) {

		LOGGER.error(
			"The value returned as response for parameter Device.IP.Interface.5.MaxMTUSize is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			e);
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 53: Validation of the Object Device.IP.Interface.5.MaxMTUSize via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 53: DESCRIPTION: Validation of the Object Device.IP.Interface.5.MaxMTUSize via WebPa ");
	    LOGGER.info(
		    "stepNumber 53: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.5.MaxMTUSize using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.5.MaxMTUSize' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 53: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s53";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 54: Verification of the object Device.IP.Interface.5.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 54: DESCRIPTION: Verification of the object Device.IP.Interface.5.Status via TR69");
	    LOGGER.info(
		    "stepNumber 54: ACTION: a) Execute getParameterName on the object Device.IP.Interface.5.Status  using the TR69 Rest API");
	    LOGGER.info("                       b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 54: EXPECTED: The value returned should be of type String and the value should be Up. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s54";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_UP);
		responseFromPreviousStep = response;
		message = "The response for the parameter Device.IP.Interface.5.Status : " + response
			+ "| Expected should be Up";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.5.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 55: Validation of the Object Device.IP.Interface.5.Status via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 55: DESCRIPTION: Validation of the Object Device.IP.Interface.5.Status via WebPa ");
	    LOGGER.info(
		    "stepNumber 55: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.5.Status using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.5.Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 55: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s55";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_5));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 56: Verification of the object Device.IP.Interface.6.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 56: DESCRIPTION: Verification of the object Device.IP.Interface.6.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 56: ACTION: a) Execute getParameterName on the object Device.IP.Interface.6.Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 56: EXPECTED: The value returned should be of type boolean and it should be false. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s56";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.FALSE));
		message = "The response for the parameter Device.IP.Interface.6.Enable : " + response
			+ "| Expected should be false";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.6.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 57: Validation of the Object Device.IP.Interface.6.Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 57: DESCRIPTION: Validation of the Object Device.IP.Interface.6.Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 57: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.6.Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.6.Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 57: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s57";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 58: Verification of the object Device.IP.Interface.6.IPv4Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 58: DESCRIPTION: Verification of the object Device.IP.Interface.6.IPv4Enable via TR69");
	    LOGGER.info(
		    "stepNumber 58: ACTION: a) Execute getParameterName on the object Device.IP.Interface.6.IPv4Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 58: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s58";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.6.IPv4Enable: " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.6.IPv4Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 59: Validation of the Object Device.IP.Interface.6.IPv4Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 59: DESCRIPTION: Validation of the Object Device.IP.Interface.6.IPv4Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 59: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.6.IPv4Enable using the command,"
			    + " \"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.6.IPv4Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 59: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s59";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV4ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 60: Verification of the object Device.IP.Interface.6.IPv6Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 60: DESCRIPTION: Verification of the object Device.IP.Interface.6.IPv6Enable via TR69");
	    LOGGER.info(
		    "stepNumber 60: ACTION: a) Execute getParameterName on the object Device.IP.Interface.6.IPv6Enable  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 60: EXPECTED: The value returned should be of type boolean and it should be true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s60";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(RDKBTestConstants.TRUE));
		message = "The response for the parameter Device.IP.Interface.6.IPv6Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.6.IPv6Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 61: Validation of the Object Device.IP.Interface.6.IPv6Enable via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 61: DESCRIPTION: Validation of the Object Device.IP.Interface.6.IPv6Enable via WebPa ");
	    LOGGER.info(
		    "stepNumber 61: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.6.IPv6Enable using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.6.IPv6Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 61: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s61";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.IPV6ENABLE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 62: Verification of the object Device.IP.Interface.6.LastChange via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 62: DESCRIPTION: Verification of the object Device.IP.Interface.6.LastChange via TR69 ");
	    LOGGER.info(
		    "stepNumber 62: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.6.LastChange using the TR69 Rest API ,"
			    + "\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.6.LastChange");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 62: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s62";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.LASTCHANGE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {

		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.IP.Interface.6.LastChange is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		message = "The response for the parameter Device.IP.Interface.6.LastChange: " + response
			+ " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.6.LastChange is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 63: Verification of the object Device.IP.Interface.6.MaxMTUSize via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 63: DESCRIPTION: Verification of the object Device.IP.Interface.6.MaxMTUSize via TR69 ");
	    LOGGER.info(
		    "stepNumber 63: ACTION: a) Execute Get Parameter value on the object Device.IP.Interface.6.MaxMTUSize using the TR69 Rest API "
			    + ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.IP.Interface.6.MaxMTUSize");
	    LOGGER.info(" 					   b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 63: EXPECTED: The value returned should be a valid unsigned integer and the value be between 64 and 66658 ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s63";
	    status = false;
	    get_param.clear();

	    try {

		get_param = BroadBandTr69Utils.getParameterForTr69Get(
			TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));
		response = tapEnv.getTR69ParameterValues(device, get_param);
		LOGGER.info("Result of get operation is:" + response);

		if (response != null) {
		    status = CommonMethods.isNotNull(response)
			    && (Integer.parseInt(response) >= 64 && Integer.parseInt(response) <= 66658);
		    responseFromPreviousStep = response;
		    message = "The response for the parameter Device.IP.Interface.6.MaxMTUSize: " + response
			    + " |Expected value should be a valid unsigned integer.";

		} else {
		    message = "No response is returned when the TR69 parameter Device.IP.Interface.6.MaxMTUSize is queried.";
		}

	    } catch (NumberFormatException e) {

		LOGGER.error(
			"The value returned as response for parameter Device.IP.Interface.6.MaxMTUSize is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			e);
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 64: Validation of the Object Device.IP.Interface.6.MaxMTUSize via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 64: DESCRIPTION: Validation of the Object Device.IP.Interface.6.MaxMTUSize via WebPa ");
	    LOGGER.info(
		    "stepNumber 64: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.6.MaxMTUSize using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.6.MaxMTUSize' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 64: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s64";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.MAXMTUSIZE.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 65: Verification of the object Device.IP.Interface.6.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 65: DESCRIPTION: Verification of the object Device.IP.Interface.6.Status via TR69");
	    LOGGER.info(
		    "stepNumber 65: ACTION: a) Execute getParameterName on the object Device.IP.Interface.6.Status  using the TR69 Rest API");
	    LOGGER.info("                       b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 65: EXPECTED: The value returned should be of type String and the value should be Up. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s65";
	    status = false;
	    get_param.clear();

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_LOWERLAYER_DOWN);
		responseFromPreviousStep = response;
		message = "The response for the parameter Device.IP.Interface.6.Status : " + response
			+ "| Expected should be down";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.IP.Interface.6.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 66: Validation of the Object Device.IP.Interface.6.Status via WebPa
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 66: DESCRIPTION: Validation of the Object Device.IP.Interface.6.Status via WebPa ");
	    LOGGER.info(
		    "stepNumber 66: ACTION: a) Execute WebPa GET command on the object Device.IP.Interface.6.Status using the command, "
			    + "\"curl --request GET --url 'WEBPA_SERVER_URL<ECM MAC>/config?names=Device.IP.Interface.6.Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 66: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s66";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    TR69DeviceInterfaceObjects.STATUS.assignIndexAndReturn(BroadBandTestConstants.CONSTANT_6));

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	} catch (Exception exception) {
	    message = exception.getMessage();
	    LOGGER.error("Exception occured while validating write access for TR69 Interface objects " + message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, message, true);
	}
    }

    /**
	* Step1: Verification of the object Device.WiFi.AccessPoint.10001.Enable via TR69
	* Step2: Validate the TR69 response * of the object Device.WiFi.AccessPoint.10001.Enable against WebPa response
	* Step3: Verification of the object * Device.WiFi.AccessPoint.10001.Status via TR69
	* Step4: Validate the TR69 response of the object * Device.WiFi.AccessPoint.10001.Status against WebPa response
	* Step5: Verification of the object * Device.WiFi.AccessPoint.10001.SSIDReference via TR69
	* Step6: Validation of the Object * Device.WiFi.AccessPoint.10001.SSIDReference via WebPa
	* Step7: Verification of the object * Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled via TR69
	* Step8: Validation of the Object * Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled via WebPa
	* Step9: Verification of the object * Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries via TR69
	* Step10: Validation of the Object * Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries via WebPa
	* Step11: Verification of the object * Device.WiFi.AccessPoint.10001.MaxAssociatedDevices via TR69
	* Step12: Validation of the Object * Device.WiFi.AccessPoint.10001.MaxAssociatedDevices via WebPa
	* Step13: Verification of the object * Device.WiFi.AccessPoint.10001.Security.ModesSupported via TR69
	* Step14: Validation of the Object * Device.WiFi.AccessPoint.10001.Security.ModesSupported via WebPa
	* Step15: Verification of the object * Device.WiFi.AccessPoint.10001.Security.ModeEnabled via TR69
	* Step16: Validation of the Object * Device.WiFi.AccessPoint.10001.Security.ModeEnabled via WebPa
	* Step17: Verification of the object * Device.WiFi.AccessPoint.10101.Enable via TR69
	* Step18: Validate the TR69 response of the object * Device.WiFi.AccessPoint.10101.Enable against WebPa response
	* Step19: Verification of the object * Device.WiFi.AccessPoint.10101.Status via TR69
	* Step20: Validate the TR69 response of the object * Device.WiFi.AccessPoint.10101.Status against WebPa response
	* Step21: Verification of the object * Device.WiFi.AccessPoint.10101.SSIDReference via TR69
	* Step22: Validation of the Object * Device.WiFi.AccessPoint.10101.SSIDReference via WebPa
	* Step23: Verification of the object * Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled via TR69
	* Step24: Validation of the Object * Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled via WebPa
	* Step25: Verification of the object * Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries via TR69
	* Step26: Validation of the Object * Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries via WebPa
	* Step27: Verification of the object * Device.WiFi.AccessPoint.10101.MaxAssociatedDevices via TR69
	* Step28: Validation of the Object * Device.WiFi.AccessPoint.10101.MaxAssociatedDevices via WebPa
	* Step29: Verification of the object * Device.WiFi.AccessPoint.10101.Security.ModesSupported via TR69
	* Step30: Validation of the Object * Device.WiFi.AccessPoint.10101.Security.ModesSupported via WebPa
	* Step31: Verification of the object * Device.WiFi.AccessPoint.10101.Security.ModeEnabled via TR69
	* Step32: Validation of the Object * Device.WiFi.AccessPoint.10101.Security.ModeEnabled via WebPa
    *
    * @author Sathurya Ravi
    * 
    * @refactor Govardhan
    * 
    * @param settop settop to be used for execution
    *
    */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1019")

    public void testValidateTR69WiFiAccessPointObjects(Dut device) {

	// stores the test result
	boolean status = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-019";
	// stores the error message
	String message = null;
	// stores the stepNumber
	String stepNumber = null;
	// stores the command response
	String response = null;
	// stores previous step response
	String responseFromPreviousStep = null;

	List<String> get_param = new ArrayList<String>();

	try {
	    LOGGER.info("STARTING TEST CASE : TC-RDKB-TR69-1019");

	    /*
	     * stepNumber 1: Verification of the object Device.WiFi.AccessPoint.10001.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 1: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10001.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 1: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10001.Enable  using the TR69 Rest API, "
			    + "<TELESCOPIC_URL>");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 1: EXPECTED: The value returned should be of type boolean. It should be either false or true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s1";
	    status = false;
	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_ENABLE);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.isBoolean(response);
		message = "The response for the parameter Device.WiFi.AccessPoint.10001.Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10001.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 2: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.Enable against WebPa
	     * response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 2: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.Enable against WebPa response ");
	    LOGGER.info(
		    "stepNumber 2: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10001.Enable using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10001.Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 2: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s2";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_ENABLE);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 3: Verification of the object Device.WiFi.AccessPoint.10001.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 3: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10001.Status via TR69");
	    LOGGER.info(
		    "stepNumber 3: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10001.Status using the TR69 Rest API,"
			    + " <TELESCOPIC_URL>=Device.WiFi.AccessPoint.10001.Status \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 3: EXPECTED: The value returned should be of type String. It should be either Enabled or Disabled. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s3";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_STATUS);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equals(BroadBandTestConstants.INTERFACE_ENABLED_STATUS)
				|| response.equals(BroadBandTestConstants.INTERFACE_DISABLED_STATUS));
		message = "The response for the parameter Device.WiFi.AccessPoint.10001.Status : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10001.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 4: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.Status against WebPa
	     * response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 4: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.Status against WebPa response ");
	    LOGGER.info(
		    "stepNumber 4: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10001.Status using the command, "
			    + "\"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10001.Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 4: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s4";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_STATUS);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 5: Verification of the object Device.WiFi.AccessPoint.10001.SSIDReference via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 5: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10001.SSIDReference via TR69 ");
	    LOGGER.info(
		    "stepNumber 5: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10001.SSIDReference using the TR69 Rest API,"
			    + " <TELESCOPIC_URL>=Device.WiFi.AccessPoint.10001.SSIDReference \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 5: EXPECTED: The value returned should be of type String. It should Device.WiFi.SSID.10001. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s5";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_SSIDREFERENCE);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equals(BroadBandTestConstants.VALUE_FOR_LOWERLAYER_DEVICE_WIFI_SSID_10001));
		message = "The response for the parameter Device.WiFi.AccessPoint.10001.SSIDReference : " + response
			+ "| Expected should be Device.WiFi.SSID.10001.";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10001.SSIDReference is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 6: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.SSIDReference
	     * against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 6: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.SSIDReference against WebPa response ");
	    LOGGER.info(
		    "stepNumber 6: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10001.SSIDReference using the command, "
			    + "\"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10001.SSIDReference' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 6: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s6";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_SSIDREFERENCE);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 7: Verification of the object Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 7: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled via TR69 ");
	    LOGGER.info(
		    "stepNumber 7: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled using the TR69 Rest API,"
			    + "<TELESCOPIC_URL>=Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 7: EXPECTED: The value returned should be of type Boolean. It should be either true or false ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s7";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_SSIDADVERTISEMENTENABLED);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.isBoolean(response);
		message = "The response for the parameter Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled : "
			+ response + "| Expected should be either true or false";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 8: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 8: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled against WebPa response ");
	    LOGGER.info(
		    "stepNumber 8: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 8: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s8";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_SSIDADVERTISEMENTENABLED);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . | Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 9: Verification of the object Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries
	     * via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 9: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries via TR69 ");
	    LOGGER.info(
		    "stepNumber 9: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries using the TR69 Rest API,"
			    + " <TELESCOPIC_URL>=Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info("stepNumber 9: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s9";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_ASSOCIATEDDEVICENUMBEROFENTRIES);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {

		try {

		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		responseFromPreviousStep = response;
		message = "The response for the parameter Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries : "
			+ response + " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "The response for Get Parameter Value on Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries is null.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 10: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 10: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries against WebPa response ");
	    LOGGER.info(
		    "stepNumber 10: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10001.AssociatedDeviceNumberOfEntries' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 10: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s10";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_ASSOCIATEDDEVICENUMBEROFENTRIES);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 11: Verification of the object Device.WiFi.AccessPoint.10001.MaxAssociatedDevices via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 11: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10001.MaxAssociatedDevices via TR69 ");
	    LOGGER.info(
		    "stepNumber 11: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10001.MaxAssociatedDevices using the TR69 Rest API, "
			    + "<TELESCOPIC_URL>=Device.WiFi.AccessPoint.10001.MaxAssociatedDevices \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info("stepNumber 11: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s11";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_MAXASSOCIATEDDEVICES);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {
		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.WiFi.AccessPoint.10001.MaxAssociatedDevices is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		responseFromPreviousStep = response;
		message = "The response for the parameter Device.WiFi.AccessPoint.10001.MaxAssociatedDevices : "
			+ response + " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "The response for Get Parameter Value on Device.WiFi.AccessPoint.10001.MaxAssociatedDevices is null.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 12: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10001.MaxAssociatedDevices against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 12: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.MaxAssociatedDevices against WebPa response ");
	    LOGGER.info(
		    "stepNumber 12: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10001.MaxAssociatedDevices using the command, "
			    + "\"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10001.MaxAssociatedDevices' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 12: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s12";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_MAXASSOCIATEDDEVICES);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 13: Verification of the object Device.WiFi.AccessPoint.10001.Security.ModesSupported via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 13: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10001.Security.ModesSupported via TR69 ");
	    LOGGER.info(
		    "stepNumber 13: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10001.Security.ModesSupported using the TR69 Rest API, "
			    + "<TELESCOPIC_URL>=Device.WiFi.AccessPoint.10001.Security.ModesSupported \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 13: EXPECTED: The value returned should be a string and value should be like supported security mode should be separated by comma ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s13";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_MODESSUPPORTED);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.matches(BroadBandTestConstants.REG_EXPRESSION_SUPPORTED_MODES));
		message = "The response for the parameter Device.WiFi.AccessPoint.10001.Security.ModesSupported : "
			+ response + "| Expected is the supported security mode should be separated by comma ";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10001.Security.ModesSupported is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 14: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10001.Security.ModesSupported against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 14: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.Security.ModesSupported against WebPa response ");
	    LOGGER.info(
		    "stepNumber 14: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10001.Security.ModesSupported using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10001.Security.ModesSupported' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 14: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s14";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_MODESSUPPORTED);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 15: Verification of the object Device.WiFi.AccessPoint.10001.Security.ModeEnabled via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 15: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10001.Security.ModeEnabled via TR69 ");
	    LOGGER.info(
		    "stepNumber 15: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10001.Security.ModeEnabled using the TR69 Rest API,"
			    + " <TELESCOPIC_URL>=Device.WiFi.AccessPoint.10001.Security.ModeEnabled \"");
	    LOGGER.info("                       b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 15: EXPECTED: The value returned should be a string and value should be like supported security mode should be separated by comma ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s15";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_MODEENABLED);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (responseFromPreviousStep.matches(".*\\b,?" + response + ",?\\b.*"));
		message = "The response for the parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled : "
			+ response + "| Expected is the supported security mode should be separated by comma ";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10001.Security.ModeEnabled is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 16: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10001.Security.ModeEnabled against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 16: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10001.Security.ModeEnabled against WebPa response ");
	    LOGGER.info(
		    "stepNumber 16: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10001.Security.ModeEnabled using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10001.Security.ModeEnabled' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					    b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 16: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s16";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4GHZ_MODEENABLED);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response .  Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 17: Verification of the object Device.WiFi.AccessPoint.10101.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 17: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10101.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 17: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10101.Enable  using the TR69 Rest API, "
			    + "<TELESCOPIC_URL>=Device.WiFi.AccessPoint.10101.Enable \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 17: EXPECTED: The value returned should be of type boolean. It should be either false or true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s17";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_ENABLE);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.isBoolean(response);
		message = "The response for the parameter Device.WiFi.AccessPoint.10101.Enable : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10101.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 18: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.Enable against
	     * WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 18: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.Enable against WebPa response ");
	    LOGGER.info(
		    "stepNumber 18: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10101.Enable using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10101.Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 18: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s18";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_ENABLE);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 19: Verification of the object Device.WiFi.AccessPoint.10101.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 19: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10101.Status via TR69");
	    LOGGER.info(
		    "stepNumber 19: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10101.Status using the TR69 Rest API,"
			    + " <TELESCOPIC_URL>=Device.WiFi.AccessPoint.10101.Status \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 19: EXPECTED: The value returned should be of type String. It should be either Enabled or Disabled. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s19";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_STATUS);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equals(BroadBandTestConstants.INTERFACE_ENABLED_STATUS)
				|| response.equals(BroadBandTestConstants.INTERFACE_DISABLED_STATUS));
		message = "The response for the parameter Device.WiFi.AccessPoint.10101.Status : " + response
			+ "| Expected should be true";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10101.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 20: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.Status against
	     * WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 20: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.Status against WebPa response ");
	    LOGGER.info(
		    "stepNumber 20: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10101.Status using the command, "
			    + "\"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10101.Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 20: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s20";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_STATUS);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 21: Verification of the object Device.WiFi.AccessPoint.10101.SSIDReference via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 21: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10101.SSIDReference via TR69 ");
	    LOGGER.info(
		    "stepNumber 21: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10101.SSIDReference using the TR69 Rest API,"
			    + " <TELESCOPIC_URL>=Device.WiFi.AccessPoint.10101.SSIDReference \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 21: EXPECTED: The value returned should be of type String. It should Device.WiFi.SSID.10101. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s21";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SSIDREFERENCE);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equals(BroadBandTestConstants.VALUE_FOR_LOWERLAYER_DEVICE_WIFI_SSID_10101));
		message = "The response for the parameter Device.WiFi.AccessPoint.10101.SSIDReference : " + response
			+ "| Expected should be Device.WiFi.SSID.10001.";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10101.SSIDReference is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 22: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.SSIDReference
	     * against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 22: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.SSIDReference against WebPa response ");
	    LOGGER.info(
		    "stepNumber 22: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10101.SSIDReference using the command, "
			    + "\"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10101.SSIDReference' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 22: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s22";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SSIDREFERENCE);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 23: Verification of the object Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 23: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled via TR69 ");
	    LOGGER.info(
		    "stepNumber 23: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled using the TR69 Rest API,"
			    + "<TELESCOPIC_URL>=Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 23: EXPECTED: The value returned should be of type Boolean. It should be either true or false ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s23";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SSIDADVERTISEMENTENABLED);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.isBoolean(response);
		message = "The response for the parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled : "
			+ response + "| Expected should be either true or false";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 24: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 24: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled against WebPa response ");
	    LOGGER.info(
		    "stepNumber 24: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 24: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s24";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SSIDADVERTISEMENTENABLED);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . | Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 25: Verification of the object Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries
	     * via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 25: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries via TR69 ");
	    LOGGER.info(
		    "stepNumber 25: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries using the TR69 Rest API,"
			    + " <TELESCOPIC_URL>=Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info("stepNumber 25: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s25";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_ASSOCIATEDDEVICENUMBEROFENTRIES);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {

		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		responseFromPreviousStep = response;
		message = "The response for the parameter Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries : "
			+ response + " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "The response for Get Parameter Value on Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries is null.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 26: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 26: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries against WebPa response ");
	    LOGGER.info(
		    "stepNumber 26: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10101.AssociatedDeviceNumberOfEntries' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 26: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s26";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_ASSOCIATEDDEVICENUMBEROFENTRIES);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 27: Verification of the object Device.WiFi.AccessPoint.10101.MaxAssociatedDevices via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 27: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10101.MaxAssociatedDevices via TR69 ");
	    LOGGER.info(
		    "stepNumber 27: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10101.MaxAssociatedDevices using the TR69 Rest API, "
			    + "<TELESCOPIC_URL>=Device.WiFi.AccessPoint.10101.MaxAssociatedDevices \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info("stepNumber 27: EXPECTED: The value returned should be a valid unsigned integer ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s27";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_MAXASSOCIATEDDEVICES);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {
		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException e) {

		    LOGGER.error(
			    "The value returned as response for parameter Device.WiFi.AccessPoint.10101.MaxAssociatedDevices is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}

		responseFromPreviousStep = response;
		message = "The response for the parameter Device.WiFi.AccessPoint.10101.MaxAssociatedDevices : "
			+ response + " |Expected value should be a valid unsigned integer.";

	    } else {
		message = "The response for Get Parameter Value on Device.WiFi.AccessPoint.10101.MaxAssociatedDevices is null.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 28: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10101.MaxAssociatedDevices against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 28: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.MaxAssociatedDevices against WebPa response ");
	    LOGGER.info(
		    "stepNumber 28: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10101.MaxAssociatedDevices using the command, "
			    + "\"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10101.MaxAssociatedDevices' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 28: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s28";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_MAXASSOCIATEDDEVICES);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 29: Verification of the object Device.WiFi.AccessPoint.10101.Security.ModesSupported via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 29: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10101.Security.ModesSupported via TR69 ");
	    LOGGER.info(
		    "stepNumber 29: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10101.Security.ModesSupported using the TR69 Rest API, "
			    + "<TELESCOPIC_URL>=Device.WiFi.AccessPoint.10101.Security.ModesSupported \"");
	    LOGGER.info("                      b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 29: EXPECTED: The value returned should be a string and value should be like supported security mode should be separated by comma ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s29";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_MODESSUPPORTED);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.matches(BroadBandTestConstants.REG_EXPRESSION_SUPPORTED_MODES));
		message = "The response for the parameter Device.WiFi.AccessPoint.10101.Security.ModesSupported : "
			+ response + "| Expected is the supported security mode should be separated by comma ";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10101.Security.ModesSupported is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 30: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10101.Security.ModesSupported against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 30: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.Security.ModesSupported against WebPa response ");
	    LOGGER.info(
		    "stepNumber 30: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10101.Security.ModesSupported using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10101.Security.ModesSupported' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 30: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s30";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_MODESSUPPORTED);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response . Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 31: Verification of the object Device.WiFi.AccessPoint.10101.Security.ModeEnabled via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 31: DESCRIPTION: Verification of the object Device.WiFi.AccessPoint.10101.Security.ModeEnabled via TR69 ");
	    LOGGER.info(
		    "stepNumber 31: ACTION: a) Execute getParameterName on the object Device.WiFi.AccessPoint.10101.Security.ModeEnabled using the TR69 Rest API,"
			    + " <TELESCOPIC_URL>=Device.WiFi.AccessPoint.10101.Security.ModeEnabled \"");
	    LOGGER.info("                       b) Validate whether the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 31: EXPECTED: The value returned should be a string and value should be like supported security mode should be separated by comma ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s31";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_MODEENABLED);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (responseFromPreviousStep.matches(".*\\b,?" + response + ",?\\b.*"));
		message = "The response for the parameter Device.WiFi.AccessPoint.10101.Security.ModeEnabled : "
			+ response + "| Expected is the supported security mode should be separated by comma ";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.AccessPoint.10101.Security.ModeEnabled is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 32: Validate the TR69 response of the object
	     * Device.WiFi.AccessPoint.10101.Security.ModeEnabled against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 32: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.AccessPoint.10101.Security.ModeEnabled against WebPa response ");
	    LOGGER.info(
		    "stepNumber 32: ACTION: a) Execute WebPa GET command on the object Device.WiFi.AccessPoint.10101.Security.ModeEnabled using the command,"
			    + " \"curl --request GET --url '<WEBPA_URL><ECM MAC>/config?names=Device.WiFi.AccessPoint.10101.Security.ModeEnabled' --header 'authorization: Bearer <SAT-TOKEN>'");
	    LOGGER.info(
		    "					    b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 32: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s32";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_MODEENABLED);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response .  Webpa: " + response + " TR69: "
		    + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);
	} catch (Exception exception) {
	    message = exception.getMessage();
	    LOGGER.error("Exception occured while validating TR69 AccessPoint objects " + message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, message, true);
	}
	LOGGER.info("ENDING TEST CASE : TC-RDKB-TR69-1019");
    }

    /**
     * Test to validate TR69 WiFi radio stats objects as per TR69 specifications
     * 
     * Step1 Verification of the object Device.WiFi.Radio.10000.Stats.BytesSent via TR69 Step2 Verification of the
     * object Device.WiFi.Radio.10000.Stats.BytesReceived via TR69 Step3 Verification of the object
     * Device.WiFi.Radio.10000.Stats.PacketsSent via TR69 Step4 Verification of the object
     * Device.WiFi.Radio.10000.Stats.PacketsReceived via TR69 Step5 Verification of the object
     * Device.WiFi.Radio.10000.Stats.ErrorsSent via TR69 Step6 Verification of the object
     * Device.WiFi.Radio.10000.Stats.ErrorsSent via TR69 Step7 Verification of the object
     * Device.WiFi.Radio.10000.Stats.DiscardPacketsSent via TR69 Step8 Verification of the object
     * Device.WiFi.Radio.10000.Stats.DiscardPacketsReceived via TR69 Step9 Verification of the object
     * Device.WiFi.Radio.10100.Stats.BytesSent via TR69 Step10 Verification of the object
     * Device.WiFi.Radio.10100.Stats.BytesReceived via TR69 Step11 Verification of the object
     * Device.WiFi.Radio.10100.Stats.PacketsSent via TR69 Step12 Verification of the object
     * Device.WiFi.Radio.10100.Stats.PacketsReceived via TR69 Step13 Verification of the object
     * Device.WiFi.Radio.10100.Stats.ErrorsSent via TR69 Step14 Verification of the object
     * Device.WiFi.Radio.10100.Stats.DiscardPacketsSent via TR69 Step15 Verification of the object
     * Device.WiFi.Radio.10100.Stats.DiscardPacketsReceived via TR69 Step16 Verification of the object
     * Device.WiFi.Radio.10100.Stats.PacketsSent via TR69
     * 
     * @author Sathurya Ravi
     * @refactor Said Hisham
     * 
     * @param device
     *            device to be used for execution
     * 
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1014")

    public void testValidateTR69WiFiRadioStatsObjects(Dut device) {

	// stores the test result
	boolean status = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-014";
	// stores the error message
	String commonMessage = "";
	// stores the stepNumber
	String stepNumber = "s1";

	try {

	    /** Steps 1 to 8 */
	    verifyRadioStatsObjects(BroadBandTestConstants.RADIO_24_GHZ_INDEX, testId, device,
		    BroadBandTestConstants.CONSTANT_1);

	    /** Steps 9 to 16 */
	    verifyRadioStatsObjects(BroadBandTestConstants.RADIO_5_GHZ_INDEX, testId, device,
		    BroadBandTestConstants.CONSTANT_9);

	} catch (Exception exception) {
	    commonMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating TR69 Radio Child objects (Stats) " + commonMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, commonMessage,
		    true);
	}

    }

    private void verifyRadioStatsObjects(String index, String testId, Dut device, int stepNumber) {
	String step = "";
	String response = "";
	String objectName = "";
	boolean status = false;
	List<String> get_param = new ArrayList<String>();
	String message = "";

	/*
	 * Verification of the object Device.WiFi.Radio.10000.Stats.BytesSent
	 */
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the  object Device.WiFi.Radio.10000.Stats.BytesSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.Radio.10000.Stats.BytesSent using the TR69 Rest API");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	String result = "";

	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATS_BYTESSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	result = tapEnv.getTR69ParameterValues(device, get_param);
	LOGGER.info("Result of get operation is:" + result);

	if (result != null) {
	    status = CommonMethods.isNotNull(result) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", result);
	    message = "The TR69 response for the parameter " + objectName + ": " + result
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.Radio.10000.Stats.BytesReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the  object Device.WiFi.Radio.10000.Stats.BytesReceived  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.Radio.10000.Stats.BytesReceived using the TR69 Rest API ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATS_BYTESRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	LOGGER.info("Result of get operation is:" + response);
	if (response != null) {
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.Radio.10000.Stats.PacketsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the  object Device.WiFi.Radio.10000.Stats.PacketsSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.Radio.10000.Stats.PacketsSent using the TR69 Rest API ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATS_PACKETSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	LOGGER.info("Result of get operation is:" + response);
	if (response != null) {
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.Radio.10000.Stats.PacketsReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the  object Device.WiFi.Radio.10000.Stats.PacketsReceived  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.Radio.10000.Stats.PacketsReceived using the TR69 Rest API ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATS_PACKETSRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	LOGGER.info("Result of get operation is:" + response);
	if (response != null) {
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.Radio.10000.Stats.ErrorsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the  object Device.WiFi.Radio.10000.Stats.ErrorsSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.Radio.10000.Stats.ErrorsSent using the TR69 Rest API ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATS_ERRORSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	LOGGER.info("Result of get operation is:" + response);
	if (response != null) {
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.Radio.10000.Stats.ErrorsReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the  object Device.WiFi.Radio.10000.Stats.ErrorsReceived  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.Radio.10000.Stats.ErrorsReceived using the TR69 Rest API");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATS_ERRORSRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	LOGGER.info("Result of get operation is:" + response);
	if (response != null) {
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.Radio.10000.Stats.DiscardPacketsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the  object Device.WiFi.Radio.10000.Stats.DiscardPacketsSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.Radio.10000.Stats.DiscardPacketsSent using the TR69 Rest API ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATS_DISCARDPACKETSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	LOGGER.info("Result of get operation is:" + response);
	if (response != null) {
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.Radio.10000.Stats.DiscardPacketsReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the  object Device.WiFi.Radio.10000.Stats.DiscardPacketsReceived  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.Radio.10000.Stats.DiscardPacketsReceived using the TR69 Rest API ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATS_DISCARDPACKETSRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	LOGGER.info("Result of get operation is:" + response);
	if (response != null) {
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);
    }

   /**
    * Test to validate the TR69 WiFi SSID objects against TR69 specifications.
    * 
	* Step1: Verification of the object Device.WiFi.SSID.10001.Enable via TR69
	* Step2: Validation of the Object Device.WiFi.SSID.10001.Enable via SNMP
	* Step3: Verification of the object Device.WiFi.SSID.10001.Status via TR69
	* Step4: Validate the TR69 response of the object Device.WiFi.SSID.10001.Status against WebPa response
	* Step5: Verification of the object Device.WiFi.SSID.10001.Alias via TR69
	* Step6: Validation of the Object Device.WiFi.SSID.10001.Alias via WebPa
	* Step7: Verification of the object Device.WiFi.SSID.10001.Name via TR69
	* Step8: Validation of the Object Device.WiFi.SSID.10001.Name via WebPa
	* Step9: Verification of the object Device.WiFi.SSID.10001.LastChange via TR69
	* Step10: Validation of the Object Device.WiFi.SSID.10001.LastChange via WebPa
	* Step11: Verification of the object Device.WiFi.SSID.10001.LowerLayers via TR69
	* Step12: Validation of the Object Device.WiFi.SSID.10001.LowerLayers via WebPa
	* Step13: Verification of the object Device.WiFi.SSID.10001.BSSID via TR69
	* Step14: Validation of the Object Device.WiFi.SSID.10001.BSSID via SNMP
	* Step15: Verification of the object Device.WiFi.SSID.10001.MACAddress via TR69
	* Step16: Validation of the Object Device.WiFi.SSID.10001.MACAddress via SNMP
	* Step17: Verification of the object Device.WiFi.SSID.10001.SSID via TR69
	* Step18: Validation of the Object Device.WiFi.SSID.10001.SSID via SNMP
	* Step19: Verification of the object Device.WiFi.SSID.10101.Enable via TR69
	* Step20: Validation of the Object Device.WiFi.SSID.10101.Enable via SNMP
	* Step21: Verification of the object Device.WiFi.SSID.10101.Status via TR69
	* Step22: Validate the TR69 response of the object Device.WiFi.SSID.10101.Status against WebPa response
	* Step23: Verification of the object Device.WiFi.SSID.10101.Alias via TR69
	* Step24: Validation of the Object Device.WiFi.SSID.10101.Alias via WebPa
	* Step25: Verification of the object Device.WiFi.SSID.10101.Name via TR69
	* Step26: Validation of the Object Device.WiFi.SSID.10101.Name via WebPa
	* Step27: Verification of the object Device.WiFi.SSID.10101.LastChange via TR69
	* Step28: Validation of the Object Device.WiFi.SSID.10101.LastChange via WebPa
	* Step29: Verification of the object Device.WiFi.SSID.10101.LowerLayers via TR69
	* Step30: Validation of the Object Device.WiFi.SSID.10101.LowerLayers via WebPa
	* Step31: Verification of the object Device.WiFi.SSID.10101.BSSID via TR69
	* Step32: Validation of the Object Device.WiFi.SSID.10101.BSSID via SNMP
	* Step33: Verification of the object Device.WiFi.SSID.10101.MACAddress via TR69
	* Step34: Validation of the Object Device.WiFi.SSID.10101.MACAddress via SNMP
	* Step35: Verification of the object Device.WiFi.SSID.10101.SSID via TR69
	* Step36: Validation of the Object Device.WiFi.SSID.10101.SSID via SNMP
    * 
    * @author Sathurya Ravi
    * @refactor Said Hisham
    * 
    * @param device
    *            device to be used for execution
    *
    */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1018")
    public void testValidateTR69WiFiSSIDObjects(Dut device) {

	// stores the test result
	boolean status = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-018";
	// stores the error message
	String message = null;
	// stores the stepNumber
	String stepNumber = null;
	// stores the command response
	String response = null;
	List<String> get_param = new ArrayList<String>();
	// stores response from previous step
	String responseFromPreviousStep = "";

	try {

	    /*
	     * stepNumber 1: Verification of the object Device.WiFi.SSID.10001.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 1: DESCRIPTION: Verification of the object Device.WiFi.SSID.10001.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 1: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10001.Enable  using the TR69 Rest API ");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 1: EXPECTED: The value returned should be of type boolean. It should be either false or true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s1";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.isBoolean(response);
		message = "The TR69 response for the parameter Device.WiFi.SSID.10001.Enable: " + response
			+ "| Expected should be either true or false";
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10001.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 2: Validation of the Object Device.WiFi.SSID.10001.Enable via SNMP
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 2: DESCRIPTION: Validation of the Object Device.WiFi.SSID.10001.Enable via SNMP");
	    LOGGER.info(
		    "stepNumber 2: ACTION: a) Execute SNMP GET command on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 using the command ");
	    LOGGER.info(
		    "                      b) Verify if the SNMP response is in line with the TR69 response from Step1");
	    LOGGER.info(
		    "stepNumber 2: EXPECTED: If the response from step 1 is true it should return 1 and if the response is false it should return 2 ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s2";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getOid(),
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_2_4_GHZ.getTableIndex());

	    if (CommonMethods.isNotNull(response)) {
		try {
		    status = ((responseFromPreviousStep.equals(AutomaticsConstants.TRUE)
			    && Integer.parseInt(response) == AutomaticsConstants.CONSTANT_1)
			    || (responseFromPreviousStep.equals(AutomaticsConstants.FALSE)
				    && Integer.parseInt(response) == AutomaticsConstants.CONSTANT_2));
		} catch (NumberFormatException nfe) {
		    status = false;
		    message = "Number format exception has occured while trying to convert the SNMP response";
		    LOGGER.error("Number format exception has occured while trying to convert the SNMP response", nfe);
		}
		message = "The response for the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 : SNMP response " + response
			+ " TR69 response: " + responseFromPreviousStep
			+ "| Expected : If the response from step 1 is true it should return 1 and if the response is false it should return 2";
	    } else {
		message = "No response is returned when the SNMP MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.2.10001 is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 3: Verification of the object Device.WiFi.SSID.10001.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 3: DESCRIPTION: Verification of the object Device.WiFi.SSID.10001.Status via TR69");
	    LOGGER.info(
		    "stepNumber 3: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10001.Status  using the TR69 Rest API ");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 3: EXPECTED: The value returned should be of type String. It should be either Up or Down. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s3";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equalsIgnoreCase(BroadBandTestConstants.STRING_UP)
				|| response.equalsIgnoreCase(BroadBandTestConstants.STRING_DOWN));
		message = "The response for the parameter Device.WiFi.SSID.10001.Status: " + response
			+ "| Expected should be either Up or Down";
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10001.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 4: Validate the TR69 response of the object Device.WiFi.SSID.10001.Status against WebPa
	     * response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 4: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.SSID.10001.Status against WebPa response ");
	    LOGGER.info(
		    "stepNumber 4: ACTION: a) Execute WebPa GET command on the object Device.WiFi.SSID.10001.Status using the command ");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 4: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s4";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_2_4GHZ_PRIVATE_SSID);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response : " + response + " TR69: " + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 5: Verification of the object Device.WiFi.SSID.10001.Alias via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 5: DESCRIPTION: Verification of the object Device.WiFi.SSID.10001.Alias via TR69");
	    LOGGER.info(
		    "stepNumber 5: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10001.Alias  using the TR69 Rest API ");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 5: EXPECTED: The value returned should be a string and it should be \"ath0\" ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s5";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_ALIAS_FOR_2_4GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		String interfaceAlias = BroadbandPropertyFileHandler.getInterfaceValues(device);
		LOGGER.info("interface" + interfaceAlias);
		status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response, interfaceAlias);
		message = "The response for the parameter Device.WiFi.SSID.10001.Alias : " + response;
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10001.Alias is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 6: Validate the TR69 response of the object Device.WiFi.SSID.10001.Alias against WebPa
	     * response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 6: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.SSID.10001.Alias against WebPa response ");
	    LOGGER.info(
		    "stepNumber 6: ACTION: a) Execute WebPa GET command on the object Device.WiFi.SSID.10001.Alias using the command ");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 6: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s6";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_ALIAS_FOR_2_4GHZ_PRIVATE_SSID);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response : " + response + " TR69: " + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 7: Verification of the object Device.WiFi.SSID.10001.Name via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 7: DESCRIPTION: Verification of the object Device.WiFi.SSID.10001.Name via TR69");
	    LOGGER.info(
		    "stepNumber 7: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10001.Name  using the TR69 Rest API ");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 7: EXPECTED: The value returned should be a string and it should be \"ath0\" ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s7";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_NAME_FOR_2_4GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		String interfaceNames = BroadbandPropertyFileHandler.getInterfaceNames(device);
		LOGGER.info("interface" + interfaceNames);
		status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response, interfaceNames);
		message = "The response for the parameter Device.WiFi.SSID.10001.Name : " + response
			+ "| Expected should be ath0";
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10001.Name is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 8: Validate the TR69 response of the object Device.WiFi.SSID.10001.Name against WebPa response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 8: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.SSID.10001.Name against WebPa response ");
	    LOGGER.info(
		    "stepNumber 8: ACTION: a) Execute WebPa GET command on the object Device.WiFi.SSID.10001.Name using the command ");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 8: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s8";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_NAME_FOR_2_4GHZ_PRIVATE_SSID);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response : " + response + " TR69: " + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 9: Verification of the object Device.WiFi.SSID.10001.LastChange via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 9: DESCRIPTION: Verification of the object Device.WiFi.SSID.10001.LastChange via TR69");
	    LOGGER.info(
		    "stepNumber 9: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10001.LastChange  using the TR69 Rest API ");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 9: EXPECTED: The value returned should be of type unsignedInt and it should have a valid integer value. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s9";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LAST_CHANGE_FOR_2_4GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {
		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException nfe) {
		    status = false;
		    message = "Number format exception has occurred while trying to convert TR69 response.";
		    LOGGER.error(message, nfe);
		}
		message = "The response for the parameter Device.WiFi.SSID.10001.LastChange : " + response
			+ "| Expected should be a valid unsigned integer";
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10001.LastChange is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 10: Verification of the object Device.WiFi.SSID.10001.LowerLayers via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 10: DESCRIPTION: Verification of the object Device.WiFi.SSID.10001.LowerLayers via TR69");
	    LOGGER.info(
		    "stepNumber 10: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10001.LowerLayers  using the TR69 Rest API ");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 10: EXPECTED: The value returned should be a string and it should be \"Device.WiFi.Radio.10000.\" ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s10";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LOWERLAYERS_FOR_2_4GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equals(BroadBandTestConstants.VALUE_FOR_LOWERLAYER_DEVICE_WIFI_RADIO_10000));
		message = "The response for the parameter Device.WiFi.SSID.10001.LowerLayers : " + response
			+ "| Expected should be Device.WiFi.Radio.10000.";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10001.LowerLayers is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 11: Validate the TR69 response of the object Device.WiFi.SSID.10001.LowerLayers against WebPa
	     * response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 11: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.SSID.10001.LowerLayers against WebPa response ");
	    LOGGER.info(
		    "stepNumber 11: ACTION: a) Execute WebPa GET command on the object Device.WiFi.SSID.10001.LowerLayers using the command ");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 11: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s11";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LOWERLAYERS_FOR_2_4GHZ_PRIVATE_SSID);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response : " + response + " TR69: " + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 12: Verification of the object Device.WiFi.SSID.10001.BSSID via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 12: DESCRIPTION: Verification of the object Device.WiFi.SSID.10001.BSSID via TR69");
	    LOGGER.info(
		    "stepNumber 12: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10001.BSSID  using the TR69 Rest API ");
	    LOGGER.info("                       b) b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 12: EXPECTED: The value returned should be a string and it should be a valid MAC address ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s12";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_BSSID_FOR_2_4GHZ);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (CommonMethods.isMacValid(response.trim()));
		message = "The response for the parameter Device.WiFi.SSID.10001.BSSID : " + response
			+ "| Expected should be a valid MAC Address";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10001.BSSID is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 13: Validation of the Object Device.WiFi.SSID.10001.BSSID via SNMP
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 13: DESCRIPTION: Validation of the Object Device.WiFi.SSID.10001.BSSID via SNMP");
	    LOGGER.info(
		    "stepNumber 13: ACTION: a) Execute SNMP GET command on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.1.10001 using the command ");
	    LOGGER.info(
		    "                      b) Verify if the MAC address returned via SNMP response is the same as the MAC returned via TR69 ");
	    LOGGER.info("stepNumber 13: EXPECTED: The SNMP response should return a valid MAC Address ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s13";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_BSSID_MAC_ADDRESS_2_4_GHZ_PRIVATE_SSID.getOid(),
		    BroadBandSnmpMib.ECM_BSSID_MAC_ADDRESS_2_4_GHZ_PRIVATE_SSID.getTableIndex());

	    if (CommonMethods.isNotNull(response)) {
		status = (response.trim().replace(" ", ":")).equalsIgnoreCase(responseFromPreviousStep);
		message = "The response for the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.1.10001: SNMP response " + response
			+ "| Expected : " + responseFromPreviousStep;
	    } else {
		message = "No response is returned when the SNMP MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.1.10001 is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 14: Verification of the object Device.WiFi.SSID.10001.MACAddress via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 14: DESCRIPTION: Verification of the object Device.WiFi.SSID.10001.MACAddress via TR69");
	    LOGGER.info(
		    "stepNumber 14: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10001.MACAddress  using the TR69 Rest API ");
	    LOGGER.info("                       b) b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 14: EXPECTED: The value returned should be a string and it should be a valid MAC address ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s14";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_MACADDRESS_FOR_2_4GHZ);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (CommonMethods.isMacValid(response.trim()));
		message = "The response for the parameter Device.WiFi.SSID.10001.MACAddress : " + response
			+ "| Expected should be a valid MAC Address";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10001.MACAddress is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 15: Validation of the Object Device.WiFi.SSID.10001.MACAddress via SNMP
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 15: DESCRIPTION: Validation of the Object Device.WiFi.SSID.10001.MACAddress via SNMP");
	    LOGGER.info(
		    "stepNumber 15: ACTION: a) Execute SNMP GET command on the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.1.10001 using the command ");
	    LOGGER.info(
		    "                      b) Verify if the MAC address returned via SNMP response is the same as the MAC returned via TR69 ");
	    LOGGER.info("stepNumber 15: EXPECTED: The SNMP response should return a valid MAC Address ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s15";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_BSSID_MAC_ADDRESS_2_4_GHZ_PRIVATE_SSID.getOid(),
		    BroadBandSnmpMib.ECM_BSSID_MAC_ADDRESS_2_4_GHZ_PRIVATE_SSID.getTableIndex());

	    if (CommonMethods.isNotNull(response)) {
		status = (response.trim().replace(" ", ":")).equalsIgnoreCase(responseFromPreviousStep);
		message = "The response for the MIB .1.3.6.1.4.1.17270.50.2.2.2.1.1.1.10001 : SNMP response " + response
			+ "| Expected : " + responseFromPreviousStep;

	    } else {
		message = "No response is returned when the SNMP Mib .1.3.6.1.4.1.17270.50.2.2.2.1.1.1.10001 is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 16: Verification of the object Device.WiFi.SSID.10001.SSID via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 16: DESCRIPTION: Verification of the object Device.WiFi.SSID.10001.SSID via TR69");
	    LOGGER.info(
		    "stepNumber 16: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10001.SSID using the TR69 Rest API ");
	    LOGGER.info("                       b) b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 16: EXPECTED: The value returned should be a string ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s16";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response);
		message = "The response for the parameter Device.WiFi.SSID.10001.SSID : " + response
			+ "| Expected should be a valid String and return the private 2.4 GHZ WiFi SSID";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10001.SSID is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 17: Validation of the Object Device.WiFi.SSID.10001.SSID via SNMP
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 17: DESCRIPTION: Validation of the Object Device.WiFi.SSID.10001.SSID via SNMP");
	    LOGGER.info(
		    "stepNumber 17: ACTION: a) Execute SNMP GET command on the MIB  .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 using the command ");
	    LOGGER.info(
		    "                       b) Verify if the SSID returned via SNMP response is the same as the SSID returned via TR69 ");
	    LOGGER.info("stepNumber 17: EXPECTED: The SNMP response should return a valid SSID ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s17";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getOid(),
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getTableIndex());

	    if (CommonMethods.isNotNull(response)) {
		status = response.trim().equals(responseFromPreviousStep);
		message = "The response for the MIB  .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 : SNMP response "
			+ response + "| Expected : " + responseFromPreviousStep;

	    } else {
		message = "No response is returned when the SNMP Mib .1.3.6.1.4.1.17270.50.2.2.2.1.1.3.10001 is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 18: Verification of the object Device.WiFi.SSID.10101.Enable via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 18: DESCRIPTION: Verification of the object Device.WiFi.SSID.10101.Enable via TR69");
	    LOGGER.info(
		    "stepNumber 18: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10101.Enable  using the TR69 Rest API ");
	    LOGGER.info("                      b) Validate whether the parameter is of type boolean.");
	    LOGGER.info(
		    "stepNumber 18: EXPECTED: The value returned should be of type boolean. It should be either false or true. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s18";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && BroadBandCommonUtils.isBoolean(response);
		message = "The TR69 response for the parameter Device.WiFi.SSID.10101.Enable: " + response
			+ "| Expected should be either true or false";
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10101.Enable is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 19: Validation of the Object Device.WiFi.SSID.10101.Enable via SNMP
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 19: DESCRIPTION: Validation of the Object Device.WiFi.SSID.10101.Enable via SNMP");
	    LOGGER.info(
		    "stepNumber 19: ACTION: a) Execute SNMP GET command on the MIB .1.3.6.1.4.1.34270.50.2.2.2.1.1.2.10101 using the command ");
	    LOGGER.info(
		    "                      b) Verify if the SNMP response is in line with the TR69 response from Step1");
	    LOGGER.info(
		    "stepNumber 19: EXPECTED: If the response from step 1 is true it should return 1 and if the response is false it should return 2 ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s19";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getOid(),
		    BroadBandSnmpMib.ECM_STATUS_PRIVATE_WIFI_5_GHZ.getTableIndex());

	    if (CommonMethods.isNotNull(response)) {
		try {
		    status = ((responseFromPreviousStep.equals(AutomaticsConstants.TRUE)
			    && Integer.parseInt(response) == AutomaticsConstants.CONSTANT_1)
			    || (responseFromPreviousStep.equals(AutomaticsConstants.FALSE)
				    && Integer.parseInt(response) == AutomaticsConstants.CONSTANT_2));
		} catch (NumberFormatException nfe) {
		    status = false;
		    message = "Number format exception has occured while trying to convert the SNMP response";
		    LOGGER.error("Number format exception has occured while trying to convert the SNMP response", nfe);
		}
		message = "The response for the MIB .1.3.6.1.4.1.34270.50.2.2.2.1.1.2.10101 : SNMP response " + response
			+ " TR69 response: " + responseFromPreviousStep
			+ "| Expected : If the response from step 1 is true it should return 1 and if the response is false it should return 2";
	    } else {
		message = "No response is returned when the SNMP MIB .1.3.6.1.4.1.34270.50.2.2.2.1.1.2.10101 is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 20: Verification of the object Device.WiFi.SSID.10101.Status via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 20: DESCRIPTION: Verification of the object Device.WiFi.SSID.10101.Status via TR69");
	    LOGGER.info(
		    "stepNumber 20: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10101.Status  using the TR69 Rest API ");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 20: EXPECTED: The value returned should be of type String. It should be either Up or Down. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s20";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equalsIgnoreCase(BroadBandTestConstants.STRING_UP)
				|| response.equalsIgnoreCase(BroadBandTestConstants.STRING_DOWN));
		message = "The response for the parameter Device.WiFi.SSID.10101.Status: " + response
			+ "| Expected should be either Up or Down";
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10101.Status is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 21: Validate the TR69 response of the object Device.WiFi.SSID.10101.Status against WebPa
	     * response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 21: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.SSID.10101.Status against WebPa response ");
	    LOGGER.info(
		    "stepNumber 21: ACTION: a) Execute WebPa GET command on the object Device.WiFi.SSID.10101.Status using the command");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 21: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s21";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_STATUS_FOR_5GHZ_PRIVATE_SSID);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response : " + response + " TR69: " + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 22: Verification of the object Device.WiFi.SSID.10101.Alias via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 22: DESCRIPTION: Verification of the object Device.WiFi.SSID.10101.Alias via TR69");
	    LOGGER.info(
		    "stepNumber 22: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10101.Alias  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 22: EXPECTED: The value returned should be a string and it should be \"ath1\" ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s22";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_ALIAS_FOR_5GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response);
		message = "The response for the parameter Device.WiFi.SSID.10101.Alias : " + response;
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10101.Alias is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 23: Validate the TR69 response of the object Device.WiFi.SSID.10101.Alias against WebPa
	     * response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 23: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.SSID.10101.Alias against WebPa response ");
	    LOGGER.info(
		    "stepNumber 23: ACTION: a) Execute WebPa GET command on the object Device.WiFi.SSID.10101.Alias using the command");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 23: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s23";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_ALIAS_FOR_5GHZ_PRIVATE_SSID);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response : " + response + " TR69: " + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 24: Verification of the object Device.WiFi.SSID.10101.Name via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 24: DESCRIPTION: Verification of the object Device.WiFi.SSID.10101.Name via TR69");
	    LOGGER.info(
		    "stepNumber 24: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10101.Name  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 24: EXPECTED: The value returned should be a string and it should be \"ath1\" ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s24";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_NAME_FOR_5GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (response.equals(BroadBandTestConstants.INTERFACE_ATH1));
		message = "The response for the parameter Device.WiFi.SSID.10101.Name : " + response
			+ "| Expected should be ath0";
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10101.Name is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 25: Validate the TR69 response of the object Device.WiFi.SSID.10101.Name against WebPa
	     * response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 25: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.SSID.10101.Name against WebPa response ");
	    LOGGER.info(
		    "stepNumber 25: ACTION: a) Execute WebPa GET command on the object Device.WiFi.SSID.10101.Name using the command");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 25: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s25";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_NAME_FOR_5GHZ_PRIVATE_SSID);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response : " + response + " TR69: " + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 26: Verification of the object Device.WiFi.SSID.10101.LastChange via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 26: DESCRIPTION: Verification of the object Device.WiFi.SSID.10101.LastChange via TR69");
	    LOGGER.info(
		    "stepNumber 26: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10101.LastChange  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 26: EXPECTED: The value returned should be of type unsignedInt and it should have a valid integer value. ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s26";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LAST_CHANGE_FOR_5GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		try {
		    status = CommonMethods.isNotNull(response) && (Integer.parseInt(response) >= 0);
		} catch (NumberFormatException nfe) {
		    status = false;
		    message = "Number format exception has occurred while trying to convert TR69 response.";
		    LOGGER.error(message, nfe);
		}
		message = "The response for the parameter Device.WiFi.SSID.10101.LastChange : " + response
			+ "| Expected should be a valid unsigned integer";
		responseFromPreviousStep = response;
	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10101.LastChange is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 27: Verification of the object Device.WiFi.SSID.10101.LowerLayers via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 27: DESCRIPTION: Verification of the object Device.WiFi.SSID.10101.LowerLayers via TR69");
	    LOGGER.info(
		    "stepNumber 27: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10101.LowerLayers  using the TR69 Rest API");
	    LOGGER.info("                      b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 27: EXPECTED: The value returned should be a string and it should be \"Device.WiFi.Radio.10100.\" ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s27";
	    status = false;

	    get_param = BroadBandTr69Utils.getParameterForTr69Get(
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LOWERLAYERS_FOR_5GHZ_PRIVATE_SSID);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response)
			&& (response.equals(BroadBandTestConstants.VALUE_FOR_LOWERLAYER_DEVICE_WIFI_RADIO_10100));
		message = "The response for the parameter Device.WiFi.SSID.10101.LowerLayers : " + response
			+ "| Expected should be Device.WiFi.Radio.10100.";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10101.LowerLayers is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 28: Validate the TR69 response of the object Device.WiFi.SSID.10101.LowerLayers against WebPa
	     * response
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 28: DESCRIPTION: Validate the TR69 response of the object Device.WiFi.SSID.10101.LowerLayers against WebPa response ");
	    LOGGER.info(
		    "stepNumber 28: ACTION: a) Execute WebPa GET command on the object Device.WiFi.SSID.10101.LowerLayers using the command");
	    LOGGER.info(
		    "					   b) Compare the WebPa response with the TR69 response from the above step.");
	    LOGGER.info("stepNumber 28: EXPECTED: Device should return same value for TR69 and WebPa");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s28";
	    status = false;

	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_LOWERLAYERS_FOR_5GHZ_PRIVATE_SSID);

	    status = CommonMethods.isNotNull(response) && CommonMethods.isNotNull(responseFromPreviousStep)
		    && response.equals(responseFromPreviousStep);
	    message = "The Webpa response and TR69 response : " + response + " TR69: " + responseFromPreviousStep;

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 29: Verification of the object Device.WiFi.SSID.10101.BSSID via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 29: DESCRIPTION: Verification of the object Device.WiFi.SSID.10101.BSSID via TR69");
	    LOGGER.info(
		    "stepNumber 29: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10101.BSSID  using the TR69 Rest API");
	    LOGGER.info("                       b) b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 29: EXPECTED: The value returned should be a string and it should be a valid MAC address ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s29";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_BSSID_FOR_5GHZ);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (CommonMethods.isMacValid(response.trim()));
		message = "The response for the parameter Device.WiFi.SSID.10101.BSSID : " + response
			+ "| Expected should be a valid MAC Address";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10101.BSSID is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 30: Validation of the Object Device.WiFi.SSID.10101.BSSID via SNMP
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 30: DESCRIPTION: Validation of the Object Device.WiFi.SSID.10101.BSSID via SNMP");
	    LOGGER.info(
		    "stepNumber 30: ACTION: a) Execute SNMP GET command on the MIB .1.3.6.1.4.1.34270.50.2.2.2.1.1.1.10101 using the command");
	    LOGGER.info(
		    "                      b) Verify if the MAC address returned via SNMP response is the same as the MAC returned via TR69 ");
	    LOGGER.info("stepNumber 30: EXPECTED: The SNMP response should return a valid MAC Address ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s30";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_BSSID_MAC_ADDRESS_5_GHZ_PRIVATE_SSID.getOid(),
		    BroadBandSnmpMib.ECM_BSSID_MAC_ADDRESS_5_GHZ_PRIVATE_SSID.getTableIndex());

	    if (CommonMethods.isNotNull(response)) {
		status = (response.trim().replace(" ", ":")).equalsIgnoreCase(responseFromPreviousStep);
		message = "The response for the MIB .1.3.6.1.4.1.34270.50.2.2.2.1.1.1.10101: SNMP response " + response
			+ "| Expected : " + responseFromPreviousStep;
	    } else {
		message = "No response is returned when the SNMP MIB .1.3.6.1.4.1.34270.50.2.2.2.1.1.1.10101 is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 31: Verification of the object Device.WiFi.SSID.10101.MACAddress via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 31: DESCRIPTION: Verification of the object Device.WiFi.SSID.10101.MACAddress via TR69");
	    LOGGER.info(
		    "stepNumber 31: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10101.MACAddress  using the TR69 Rest API");
	    LOGGER.info("                       b) b) Validate the type and value of the parameter");
	    LOGGER.info(
		    "stepNumber 31: EXPECTED: The value returned should be a string and it should be a valid MAC address ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s31";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_MACADDRESS_FOR_5GHZ);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response) && (CommonMethods.isMacValid(response.trim()));
		message = "The response for the parameter Device.WiFi.SSID.10101.MACAddress : " + response
			+ "| Expected should be a valid MAC Address";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10101.MACAddress is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 32: Validation of the Object Device.WiFi.SSID.10101.MACAddress via SNMP
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "stepNumber 32: DESCRIPTION: Validation of the Object Device.WiFi.SSID.10101.MACAddress via SNMP");
	    LOGGER.info(
		    "stepNumber 32: ACTION: a) Execute SNMP GET command on the MIB .1.3.6.1.4.1.34270.50.2.2.2.1.1.1.10101 using the command");
	    LOGGER.info(
		    "                      b) Verify if the MAC address returned via SNMP response is the same as the MAC returned via TR69 ");
	    LOGGER.info("stepNumber 32: EXPECTED: The SNMP response should return a valid MAC Address ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s32";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_BSSID_MAC_ADDRESS_5_GHZ_PRIVATE_SSID.getOid(),
		    BroadBandSnmpMib.ECM_BSSID_MAC_ADDRESS_5_GHZ_PRIVATE_SSID.getTableIndex());

	    if (CommonMethods.isNotNull(response)) {
		status = (response.trim().replace(" ", ":")).equalsIgnoreCase(responseFromPreviousStep);
		message = "The response for the MIB .1.3.6.1.4.1.34270.50.2.2.2.1.1.1.10101 : SNMP response " + response
			+ "| Expected : " + responseFromPreviousStep;

	    } else {
		message = "No response is returned when the SNMP Mib .1.3.6.1.4.1.34270.50.2.2.2.1.1.1.10101 is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);

	    /*
	     * stepNumber 33: Verification of the object Device.WiFi.SSID.10101.SSID via TR69
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 33: DESCRIPTION: Verification of the object Device.WiFi.SSID.10101.SSID via TR69");
	    LOGGER.info(
		    "stepNumber 33: ACTION: a) Execute getParameterName on the object Device.WiFi.SSID.10101.SSID using the TR69 Rest API");
	    LOGGER.info("                       b) b) Validate the type and value of the parameter");
	    LOGGER.info("stepNumber 33: EXPECTED: The value returned should be a string ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s33";
	    status = false;

	    get_param = BroadBandTr69Utils
		    .getParameterForTr69Get(BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME);
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    LOGGER.info("Result of get operation is:" + response);

	    if (response != null) {
		status = CommonMethods.isNotNull(response);
		message = "The response for the parameter Device.WiFi.SSID.10101.SSID : " + response
			+ "| Expected should be a valid String and return the private 5 GHZ WiFi SSID";
		responseFromPreviousStep = response;

	    } else {
		message = "No response is returned when the TR69 parameter Device.WiFi.SSID.10101.SSID is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, true);

	    /*
	     * stepNumber 34: Validation of the Object Device.WiFi.SSID.10101.SSID via SNMP
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("stepNumber 34: DESCRIPTION: Validation of the Object Device.WiFi.SSID.10101.SSID via SNMP");
	    LOGGER.info(
		    "stepNumber 34: ACTION: a) Execute SNMP GET command on the MIB  .1.3.6.1.4.1.34270.50.2.2.2.1.1.3.10101 using the command");
	    LOGGER.info(
		    "                       b) Verify if the SSID returned via SNMP response is the same as the SSID returned via TR69 ");
	    LOGGER.info("stepNumber 34: EXPECTED: The SNMP response should return a valid SSID ");
	    LOGGER.info("**********************************************************************************");

	    stepNumber = "s34";
	    status = false;

	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getOid(),
		    BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_5.getTableIndex());

	    if (CommonMethods.isNotNull(response)) {
		status = response.trim().equals(responseFromPreviousStep);
		message = "The response for the MIB  .1.3.6.1.4.1.34270.50.2.2.2.1.1.3.10101 : SNMP response "
			+ response + "| Expected : " + responseFromPreviousStep;

	    } else {
		message = "No response is returned when the SNMP Mib .1.3.6.1.4.1.34270.50.2.2.2.1.1.3.10101 is queried.";
	    }

	    LOGGER.info("Actual Status : " + message);

	    tapEnv.updateExecutionStatus(device, testId, stepNumber, status, message, false);
	} catch (Exception exception) {
	    message = exception.getMessage();
	    LOGGER.error("Exception occured while validating TR69 WiFi SSID objects " + message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, message, true);
	}
    }

   /**
    * Step1: Verification of the object Device.WiFi.Radio.10000.Enable via TR69
	* Step2: Validation of the Object * Device.WiFi.Radio.10000.Enable via WebPa
	* Step3: Verification of the object Device.WiFi.Radio.10000.Status via * TR69
	* Step4: Validate the TR69 response of the object Device.WiFi.Radio.10000.Status against WebPa response
	* Step5: * Verification of the object Device.WiFi.Radio.10000.Alias via TR69
	* Step6: Validation of the Object * Device.WiFi.Radio.10000.Alias via WebPa
	* Step7: Verification of the object Device.WiFi.Radio.10000.Name via TR69 *
	* Step8: Validation of the Object Device.WiFi.Radio.10000.Name via WebPa
	* Step9: Verification of the object * Device.WiFi.Radio.10000.LastChange via TR69
	* Step10: Validation of the Object Device.WiFi.Radio.10000.LastChange * via WebPa
	* Step11: Verification of the object Device.WiFi.Radio.10000.LowerLayers via TR69
	* Step12: Validation of * the Object Device.WiFi.Radio.10000.LowerLayers via WebPa
	* Step13: Verification of the object * Device.WiFi.Radio.10000.Upstream via TR69
	* Step14: Validation of the Object Device.WiFi.Radio.10000.Upstream via * WebPa
	* Step15: Verification of the object Device.WiFi.Radio.10000.MaxBitRate via TR69
	* Step16: Validation of the * Object Device.WiFi.Radio.10000.MaxBitRate via WebPa
	* Step17: Verification of the object * Device.WiFi.Radio.10000.SupportedFrequencyBands via TR69
	* Step18: Validation of the Object * Device.WiFi.Radio.10000.SupportedFrequencyBands via WebPa
	* Step19: Verification of the object * Device.WiFi.Radio.10000.OperatingChannelBandwidth via TR69
	* Step20: Validation of the Object * Device.WiFi.Radio.10000.OperatingChannelBandwidth via WebPa
	* Step21: Verification of the object * Device.WiFi.Radio.10000.SupportedStandards via TR69
	* Step22: Validation of the Object * Device.WiFi.Radio.10000.SupportedStandards via WebPa
	* Step23: Verification of the object * Device.WiFi.Radio.10000.OperatingStandards via TR69
	* Step24: Validation of the Object * Device.WiFi.Radio.10000.OperatingStandards via WebPa
	* Step25: Verification of the object * Device.WiFi.Radio.10100.Enable via TR69
	* Step26: Validation of the Object Device.WiFi.Radio.10100.Enable via WebPa *
	* Step27: Verification of the object Device.WiFi.Radio.10100.Status via TR69
	* Step28: Validation of the Object * Device.WiFi.Radio.10100.Status via WebPa
	* Step29: Verification of the object Device.WiFi.Radio.10100.Alias via * TR69
	* Step30: Validation of the Object Device.WiFi.Radio.10100.Alias via WebPa
	* Step31: Verification of the object * Device.WiFi.Radio.10100.Name via TR69
	* Step32: Validation of the Object Device.WiFi.Radio.10100.Name via WebPa *
	* Step33: Verification of the object Device.WiFi.Radio.10100.LastChange via TR69
	* Step34: Validation of the Object * Device.WiFi.Radio.10100.LastChange via WebPa
	* Step35: Verification of the object * Device.WiFi.Radio.10100.LowerLayers via TR69
	* Step36: Validation of the Object Device.WiFi.Radio.10100.LowerLayers * via WebPa
	* Step37: Verification of the object Device.WiFi.Radio.10100.Upstream via TR69
	* Step38: Validation of the * Object Device.WiFi.Radio.10100.Upstream via WebPa
	* Step39: Verification of the object * Device.WiFi.Radio.10100.MaxBitRate via TR69
	* Step40: Validation of the Object Device.WiFi.Radio.10100.MaxBitRate * via WebPa
	* Step41: Verification of the object Device.WiFi.Radio.10100.SupportedFrequencyBands via TR69
	* Step42: * Validation of the Object Device.WiFi.Radio.10100.SupportedFrequencyBands via WebPa
	* Step43: Verification of the * object Device.WiFi.Radio.10100.OperatingChannelBandwidth via TR69
	* Step44: Validation of the Object * Device.WiFi.Radio.10100.OperatingChannelBandwidth via WebPa
	* Step45: Verification of the object * Device.WiFi.Radio.10100.SupportedStandards via TR69
	* Step46: Validation of the Object * Device.WiFi.Radio.10100.SupportedStandards via WebPa
    *
    * @author Sathurya Ravi
    * @refactor Said Hisham
    * 
    * @param device
    *            Dut to be used for execution
    * 
    */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1012")

    public void testValidateTR69WiFiRadioObjects(Dut device) {

	// stores the test result
	boolean status = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-012";
	// stores the error message
	String message = "";
	// stores the stepNumber
	String stepNumber = "s1";

	try {
	    /** Step 1 to Step 23 */
	    verifyRadioObjectsForTR69(BroadBandTestConstants.RADIO_24_GHZ_INDEX, BroadBandTestConstants.CONSTANT_1,
		    device, testId);
	    /** Step 24 to Step 46 */
	    verifyRadioObjectsForTR69(BroadBandTestConstants.RADIO_5_GHZ_INDEX, BroadBandTestConstants.CONSTANT_24,
		    device, testId);
	} catch (Exception exception) {
	    message = exception.getMessage();
	    LOGGER.error("Exception occured while validating TR69 Radio frequency objects " + message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, message, true);
	}

    }

    private void verifyRadioObjectsForTR69(String index, int stepNumber, Dut device, String testId) {

	String step = "";
	boolean status = false;
	String response = "";
	String message = "";
	String objectName = "";
	List<String> get_param = new ArrayList<String>();

	/*
	 * Verification of the object Device.WiFi.Radio.{i}.Enable via TR69
	 */
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.Enable via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION:  Execute Get Parameter value on the object Device.WiFi.Radio.{i}.Enable using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.Enable");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be of type boolean. It should be either false or true. ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_WIFI_RADIO_ENABLE
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {
	    status = BroadBandCommonUtils.isBoolean(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " Expected value: "
		    + BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			    BroadBandTestConstants.FALSE);
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on Device.WiFi.Radio.{i}.Enable is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validation of the Object Device.WiFi.Radio.{i}.Enable via WebPa
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.Enable via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.Enable using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.Enable' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					   b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.isBoolean(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ " Expected value: should be true or false";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.Radio.{i}.Status via TR69
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.Status via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.Status using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.Status");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be either \"Up\" or \"Down\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_STATUS
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = CommonMethods.isNotNull(response) && BroadBandTestConstants.STATUS_VALUES.contains(response);
	    message = "The TR69 response for the parameter " + objectName + " : " + response + "| Expected: "
		    + BroadBandTestConstants.STATUS_VALUES.toString();
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of the object Device.WiFi.Radio.{i}.Status against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.Status via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.Status using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.Status' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					   b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandTestConstants.STATUS_VALUES.contains(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected value: "
		+ BroadBandTestConstants.STATUS_VALUES;
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.Alias
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.Alias via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.Alias using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.Alias");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be \"Radio0\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_ALIAS
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);

	if (response != null) {

	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    (index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.STRING_RADIO_0
			    : BroadBandTestConstants.STRING_RADIO_1),
		    response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " Expected: "
		    + (index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.STRING_RADIO_0
			    : BroadBandTestConstants.STRING_RADIO_1);
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}

	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.Alias against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.Alias via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.Alias using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.Alias' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					   b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");

	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		(index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.STRING_RADIO_0
			: BroadBandTestConstants.STRING_RADIO_1),
		response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected value: "
		+ (index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.STRING_RADIO_0
			: BroadBandTestConstants.STRING_RADIO_1);
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.Name
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.Name via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.Name using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.Name");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be \"wifi0\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	String radioName = "";
	Boolean is24GHz = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_NAME
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    is24GHz = index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX);
	    if (is24GHz) {
		radioName = BroadbandPropertyFileHandler.getRadioName24BasedOnModel(device);
	    } else {
		radioName = BroadbandPropertyFileHandler.getRadioName5BasedOnModel(device);
	    }
	    LOGGER.info("the radio name obtained for properties :" + radioName);// added
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON, radioName,
		    response);
	    message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected value: "
		    + radioName;
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.Name against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.Name via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.Name using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.Name' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					   b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON, radioName,
		response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected value: "
		+ radioName;

	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.LastChange
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.LastChange via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.LastChange using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.LastChange");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be of type unsignedInt and it should have a valid integer value.");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_LAST_CHANGE
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "",
		    response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + "| Expected: the value should be a valid unsigned integer.";
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.LowerLayers
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.LowerLayers via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.LowerLayers using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.LowerLayers");
	LOGGER.info(
		" 					   b) Validate the whether parameter is of type and value of the parameter ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be \"Not Applicable\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_LOWERLAYERS
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);

	if (response != null) {
	    status = CommonMethods.isNotNull(response)
		    && BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
			    BroadBandTestConstants.NOTAPPLICABLE_VALUE, response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " |Expected: "
		    + BroadBandTestConstants.NOTAPPLICABLE_VALUE;
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.LowerLayers against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.LowerLayers via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.LowerLayers using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.LowerLayers' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		BroadBandTestConstants.NOTAPPLICABLE_VALUE, response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected value: "
		+ BroadBandTestConstants.NOTAPPLICABLE_VALUE;
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.Upstream
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.Upstream via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.Upstream using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.Upstream");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be either true or false ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_UPSTREAM
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {
	    status = BroadBandCommonUtils.isBoolean(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + "| Expected: "
		    + BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			    BroadBandTestConstants.FALSE);
	    LOGGER.error("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.info("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.Upstream against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.Upstream via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.Upstream using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.Upstream' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.isBoolean(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected: "
		+ BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			BroadBandTestConstants.FALSE);
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.MaxBitRate
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.MaxBitRate via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.MaxBitRate using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.MaxBitRate");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_MAXBITRATE
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "",
		    response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value for parameter " + objectName + " should be a valid unsigned integer.";
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.MaxBitRate against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.MaxBitRate via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.MaxBitRate using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.MaxBitRate' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "",
		response);
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ " |Expected value for parameter " + objectName + " should be a valid unsigned integer.";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.SupportedFrequencyBands
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.SupportedFrequencyBands via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.SupportedFrequencyBands using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.SupportedFrequencyBands");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be \"2.4GHz\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_SUPPORTEDFREQUENCYBANDS
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);

	if (response != null) {
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    (index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.BAND_2_4GHZ
			    : BroadBandTestConstants.BAND_5GHZ),
		    response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " | Expected: "
		    + (index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.BAND_2_4GHZ
			    : BroadBandTestConstants.BAND_5GHZ);
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.SupportedFrequencyBands against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.SupportedFrequencyBands via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.SupportedFrequencyBands using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.SupportedFrequencyBands' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		(index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.BAND_2_4GHZ
			: BroadBandTestConstants.BAND_5GHZ),
		response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + "  Expected: "
		+ (index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.BAND_2_4GHZ
			: BroadBandTestConstants.BAND_5GHZ);
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.OperatingFrequencyBand
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.OperatingFrequencyBand via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.OperatingFrequencyBand using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.OperatingFrequencyBand");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be \"2.4GHz\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_OPERATINGFREQUENCYBAND
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    (index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.BAND_2_4GHZ
			    : BroadBandTestConstants.BAND_5GHZ),
		    response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + "|Expected: "
		    + (index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.BAND_2_4GHZ
			    : BroadBandTestConstants.BAND_5GHZ);
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.OperatingFrequencyBand against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.OperatingFrequencyBand via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.OperatingFrequencyBand using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.OperatingFrequencyBand' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		(index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.BAND_2_4GHZ
			: BroadBandTestConstants.BAND_5GHZ),
		response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected: "
		+ (index.equals(BroadBandTestConstants.RADIO_24_GHZ_INDEX) ? BroadBandTestConstants.BAND_2_4GHZ
			: BroadBandTestConstants.BAND_5GHZ);
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.SupportedStandards
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.SupportedStandards via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.SupportedStandards using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.SupportedStandards");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be \"b,g,n\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_SUPPORTEDSTANDARDS
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {
	    status = CommonMethods.isNotNull(response)
		    && BroadBandTestConstants.WIFI_OPERATING_MODES.contains(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " |Expected: "
		    + BroadBandTestConstants.WIFI_OPERATING_MODES.toString();
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.SupportedStandards against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.SupportedStandards via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.SupportedStandards using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.SupportedStandards' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = CommonMethods.isNotNull(response) && BroadBandTestConstants.WIFI_OPERATING_MODES.contains(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected: "
		+ BroadBandTestConstants.WIFI_OPERATING_MODES.toString();
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.OperatingStandards
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.OperatingStandards via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.OperatingStandards using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.OperatingStandards");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be with in the range \"b,g,n\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_OPERATINGSTANDARDS
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {
	    status = CommonMethods.isNotNull(response)
		    && BroadBandTestConstants.WIFI_OPERATING_MODES.contains(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " |Expected: "
		    + BroadBandTestConstants.WIFI_OPERATING_MODES.toString();
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validation of the Object Device.WiFi.Radio.{i}.OperatingStandards via WebPa
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Device.WiFi.Radio.{i}.OperatingStandards via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.OperatingStandards using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.OperatingStandards' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = CommonMethods.isNotNull(response) && BroadBandTestConstants.WIFI_OPERATING_MODES.contains(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected: "
		+ BroadBandTestConstants.WIFI_OPERATING_MODES.toString();
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);
    }
    
    /**
     * Validate ethernet interface entries using Tr69
     * <ol>
     * <li>PRE-CONDITION : DESCRIPTION : Enable the TR69 configuration</li>
     * <li>Retrieve Ethernet Interface entries using TR69 TELESCOPIC SERVICE REST URL</li>
     * <li>Retrieve Ethernet Interface entries using webpa and validate with tr69 response</li>
     * <li>Retrieve and validate values for ethernet interface \"Enable\" using TR69 TELESCOPIC SERVICE REST URL for all
     * entries</li>
     * <li>Retrieve and validate values for ethernet interface \"Status\" using TR69 TELESCOPIC SERVICE REST URL for all
     * entries</li>
     * <li>Retrieve and validate values for ethernet interface \"Alias\" using TR69 TELESCOPIC SERVICE REST URL for all
     * entries</li>
     * <li>Retrieve and validate values for ethernet interface \"Name\" using TR69 TELESCOPIC SERVICE REST URL for all
     * entries</li>
     * <li>Retrieve and validate values for ethernet interface \"MACAddress\" using TR69 TELESCOPIC SERVICE REST URL for
     * all entries</li>
     * <li>Retrieve and validate values for ethernet interface \"MaxBitRate\" using TR69 TELESCOPIC SERVICE REST URL for
     * all entries</li>
     * <li>Retrieve and validate values for ethernet interface \"BytesSent\" using TR69 TELESCOPIC SERVICE REST URL for
     * all entries</li>
     * <li>Retrieve and validate values for ethernet interface \"BytesReceived\" using TR69 TELESCOPIC SERVICE REST URL
     * for all entries</li>
     * <li>Retrieve and validate values for ethernet interface \"PacketsReceived\" using TR69 TELESCOPIC SERVICE REST
     * URL for all entries</li>
     * <li>Retrieve and validate values for ethernet interface \"ErrorsReceived\" using TR69 TELESCOPIC SERVICE REST URL
     * for all entries</li>
     * <li>Retrieve and validate values for ethernet interface \"DiscardPacketsSent\" using TR69 TELESCOPIC SERVICE REST
     * URL for all entries</li>
     * <li>Retrieve and validate values for ethernet interface \"BroadcastPacketsSent\" using TR69 TELESCOPIC SERVICE
     * REST URL for all entries</li>
     * <li>Retrieve and validate values for ethernet interface \"BroadcastPacketsReceived\" using TR69 TELESCOPIC
     * SERVICE REST URL for all entries</li>
     * <li>POST-CONDITION : DESCRIPTION : Disable the TR69 configuration</li>
     * </ol>
     *
     * @param device
     *
     * @author prasanthreddy.a
     * @refactor yamini.s
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-ETHNT-INTFC-1001")
    public void testToValidateTr69EthernetInterface(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-TR69-ETHNT-INTFC-101";
	String stepNum = "";
	stepNumber = 1;
	stepNum = "S" + stepNumber;
	String errorMessage = "";
	List<String> get_param = new ArrayList<String>();
	String objectName = "";
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-TR69-ETHNT-INTFC-1001");
	LOGGER.info("TEST DESCRIPTION: Validate ethernet interface entries using Tr69 ");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION : Enable the TR69 configuration ");
	LOGGER.info("1. Retrieve Ethernet Interface entries using TR69 TELESCOPIC SERVICE REST URL");
	LOGGER.info("2. Retrieve Ethernet Interface entries using webpa and validate with tr69 response");
	LOGGER.info(
		"3. Retrieve and validate values for ethernet interface \"Enable\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"4. Retrieve and validate values for ethernet interface \"Status\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"5. Retrieve and validate values for ethernet interface \"Alias\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"6. Retrieve and validate values for ethernet interface \"Name\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"7. Retrieve and validate values for ethernet interface \"MACAddress\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"8. Retrieve and validate values for ethernet interface \"MaxBitRate\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"9. Retrieve and validate values for ethernet interface \"BytesSent\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"10. Retrieve and validate values for ethernet interface \"BytesReceived\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"11. Retrieve and validate values for ethernet interface \"PacketsReceived\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"12. Retrieve and validate values for ethernet interface \"ErrorsReceived\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"13. Retrieve and validate values for ethernet interface \"DiscardPacketsSent\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info(
		"14. Retrieve and validate values for ethernet interface \"BroadcastPacketsSent\" using TR69 TELESCOPIC SERVICE  REST URL for all entries ");
	LOGGER.info(
		"15. Retrieve and validate values for ethernet interface \"BroadcastPacketsReceived\" using TR69 TELESCOPIC SERVICE REST URL for all entries ");
	LOGGER.info("POST-CONDITION : Disable the TR69 configuration ");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    BroadBandPreConditionUtils.executePreConditionToEnableTR69Configuration(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

	    /**
	     * STEP 1 : Retrieve Ethernet Interface entries using TR69 TELESCOPIC SERVICE REST URL
	     */
	    errorMessage = "Unable to retrieve value using Tr69";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Retrieve Ethernet Interface entries using TR69 TELESCOPIC SERVICE REST URL");
	    LOGGER.info(
		    "STEP " + stepNumber + ": ACTION : Param to retrieve Device.Ethernet.InterfaceNumberOfEntries ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Value should be retrieved successfully using tr69");
	    LOGGER.info("**********************************************************************************");
	    String tr69Response = null;
	    try {

		objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_ENTRIES;
		get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
		tr69Response = tapEnv.getTR69ParameterValues(device, get_param);
		LOGGER.info("Output from Tr69 :" + tr69Response);
		status = CommonMethods.isNotNull(tr69Response);
	    } catch (Exception e) {
		LOGGER.error("Exception caught while retriving response from TR69 " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully retrieved response from Tr69");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 2 : Retrieve Ethernet Interface entries using webpa and validate with tr69 response
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Unable to retrieve value using webpa";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Retrieve Ethernet Interface entries using webpa and validate with tr69 response");
	    LOGGER.info(
		    "STEP " + stepNumber + ": ACTION : Param to retrieve Device.Ethernet.InterfaceNumberOfEntries ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Value should be retrieved successfully using webpa");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_ENTRIES, tr69Response);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully retrieved response from Webpa and verified with Webpa response");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    // Parameters for response validation with Webpa response
	    String[] params = new String[] { BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_ENABLE,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_STATUS,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_ALIAS,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_NAME,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_MACADDRESS,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_MAXBITRATE,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_BYTE_SENT,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_BYTE_RECEIVED,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_PACKETS_RECEIVED,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_ERRORS_RECEIVED,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_DISCARD_PACKETS_SENT,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_BROADCAST_PACKETS_SENT,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHERNET_INTERFACE_BROADCAST_PACKETS_RECEIVED };
	    String[] responseRegex = new String[] { BroadBandTestConstants.PATTERN_TRUE_OR_FALSE,
		    BroadBandTestConstants.PATTERN_FOR_ETHERNET_INTERFACE_STATUS,
		    BroadBandTestConstants.PATTERN_ONE_LETTER_REST_CHARACTERS,
		    BroadBandTestConstants.PATTERN_ONE_LETTER_REST_CHARACTERS,
		    BroadBandTestConstants.PATTERN_MAC_VALIDATION, BroadBandTestConstants.PATTERN_IS_DIGIT,
		    BroadBandTestConstants.PATTERN_20_NUMBERS, BroadBandTestConstants.PATTERN_20_NUMBERS,
		    BroadBandTestConstants.PATTERN_20_NUMBERS, BroadBandTestConstants.PATTERN_10_NUMBERS,
		    BroadBandTestConstants.PATTERN_10_NUMBERS, BroadBandTestConstants.PATTERN_20_NUMBERS,
		    BroadBandTestConstants.PATTERN_20_NUMBERS };
	    int numberOfEntries = Integer.parseInt(tr69Response);
	    String[] paramForTr69 = new String[numberOfEntries];

	    for (int i = 0; i < params.length; i++) {
		for (int count = 0; count < numberOfEntries; count++) {
		    paramForTr69[count] = params[i].replace(BroadBandTestConstants.TR181_NODE_REF,
			    String.valueOf(count + 1));
		}
		stepNumber++;
		stepNum = "S" + stepNumber;
		errorMessage = "Unable to retrieve value using Tr69";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP " + stepNumber + ": DESCRIPTION : Retrieve and validate values for ethernet interface "
				+ params[i] + " using TR69 TELESCOPIC SERVICE REST URL for all entries ");
		LOGGER.info("STEP " + stepNumber + ": ACTION : Execute TR69 Params:" + params[i]);
		LOGGER.info("STEP " + stepNumber
			+ ": EXPECTED : Value should be retrieved using tr69 and validated successfully ");
		LOGGER.info("**********************************************************************************");
		List<String> tr69WithParameterResponse = tapEnv.getTr69ParameterValue(device, paramForTr69);
		int counter = 0;
		for (int count = 0; count < numberOfEntries; count++) {
		    String responseValue = tr69WithParameterResponse.get(count);
		    status = CommonMethods.isNotNull(responseValue)
			    && CommonMethods.patternMatcher(responseValue, responseRegex[i]);
		    counter = status ? counter + 1 : counter;

		}

		if (status) {
		    LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully validated tr69 response ");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    BroadBandPostConditionUtils.postConditionToDisableTR069Configuration(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-TR69-ETHNT-INTFC-1001");
    }
    
    /**
	* Step1 Verification of the object object Device.WiFi.Radio.10000.PossibleChannels via TR69
	* Step2 Validation of the * Object object Device.WiFi.Radio.10000.PossibleChannels via WebPa
	* Step3 Verification of the object object * Device.WiFi.Radio.10000.ChannelsInUse via TR69
	* Step4 Validation of the Object object * Device.WiFi.Radio.10000.ChannelsInUse via WebPa
	* Step5 Verification of the object object * Device.WiFi.Radio.10000.Channel via TR69
	* Step6 Validation of the Object object Device.WiFi.Radio.10000.Channel * via WebPa
	* Step7 Verification of the object object Device.WiFi.Radio.10000.AutoChannelSupported via TR69
	* Step8 * Validation of the Object object Device.WiFi.Radio.10000.AutoChannelSupported via WebPa
	* Step9 Verification of the * object object Device.WiFi.Radio.10000.AutoChannelEnable via TR69
	* Step10 Validation of the Object object * Device.WiFi.Radio.10000.AutoChannelEnable via WebPa
	* Step11 Verification of the object object * Device.WiFi.Radio.10000.AutoChannelRefreshPeriod via TR69
	* Step12 Validation of the Object object * Device.WiFi.Radio.10000.AutoChannelRefreshPeriod via WebPa
	* Step13 Verification of the object object * Device.WiFi.Radio.10000.OperatingChannelBandwidth via TR69
	* Step14 Validation of the Object object * Device.WiFi.Radio.10000.OperatingChannelBandwidth via WebPa
	* Step15 Verification of the object object * Device.WiFi.Radio.10000.ExtensionChannel via TR69
	* Step16 Validation of the Object object * Device.WiFi.Radio.10000.ExtensionChannel via WebPa
	* Step17 Verification of the object object * Device.WiFi.Radio.10000.GuardInterval via TR69
	* Step18 Validation of the Object object * Device.WiFi.Radio.10000.GuardInterval via WebPa
	* Step19 Verification of the object object * Device.WiFi.Radio.10000.MCS via TR69
	* Step20 Validation of the Object object Device.WiFi.Radio.10000.MCS via WebPa *
	* Step21 Verification of the object object Device.WiFi.Radio.10000.TransmitPowerSupported via TR69
	* Step22 * Validation of the Object object Device.WiFi.Radio.10000.TransmitPowerSupported via WebPa
	* Step23 Verification of * the object object Device.WiFi.Radio.10000.TransmitPower via TR69
	* Step24 Validation of the Object object * Device.WiFi.Radio.10000.TransmitPower via WebPa
	* Step25 Verification of the object object * Device.WiFi.Radio.10000.IEEE80211hSupported via TR69
	* Step26 Validation of the Object object * Device.WiFi.Radio.10000.IEEE80211hSupported via WebPa
	* Step27 Verification of the object object * Device.WiFi.Radio.10000.IEEE80211hEnabled via TR69
	* Step28 Validation of the Object object * Device.WiFi.Radio.10000.IEEE80211hEnabled via WebPa
	* Step29 Verification of the object object * Device.WiFi.Radio.10000.RegulatoryDomain via TR69
	* Step30 Validation of the Object object * Device.WiFi.Radio.10000.RegulatoryDomain via WebPa
	* Step31 Verification of the object object * Device.WiFi.Radio.10100.PossibleChannels via TR69
	* Step32 Validation of the Object object * Device.WiFi.Radio.10100.PossibleChannels via WebPa
	* Step33 Verification of the object object * Device.WiFi.Radio.10100.ChannelsInUse via TR69
	* Step34 Validation of the Object object * Device.WiFi.Radio.10100.ChannelsInUse via WebPa
	* Step35 Verification of the object object * Device.WiFi.Radio.10100.Channel via TR69
	* Step36 Validation of the Object object Device.WiFi.Radio.10100.Channel * via WebPa
	* Step37 Verification of the object object Device.WiFi.Radio.10100.AutoChannelSupported via TR69
	* Step38 * Validation of the Object object Device.WiFi.Radio.10100.AutoChannelSupported via WebPa
	* Step39 Verification of the * object object Device.WiFi.Radio.10100.AutoChannelEnable via TR69
	* Step40 Validation of the Object object * Device.WiFi.Radio.10100.AutoChannelEnable via WebPa
	* Step41 Verification of the object object * Device.WiFi.Radio.10100.AutoChannelRefreshPeriod via TR69
	* Step42 Validation of the Object object * Device.WiFi.Radio.10100.AutoChannelRefreshPeriod via WebPa
	* Step43 Verification of the object object * Device.WiFi.Radio.10100.OperatingChannelBandwidth via TR69
	* Step44 Validation of the Object object * Device.WiFi.Radio.10100.OperatingChannelBandwidth via WebPa
	* Step45 Verification of the object object * Device.WiFi.Radio.10100.ExtensionChannel via TR69
	* Step46 Validation of the Object object * Device.WiFi.Radio.10100.ExtensionChannel via WebPa
	* Step47 Verification of the object object * Device.WiFi.Radio.10100.GuardInterval via TR69
	* Step48 Validation of the Object object * Device.WiFi.Radio.10100.GuardInterval via WebPa
	* Step49 Verification of the object object * Device.WiFi.Radio.10100.MCS via TR69
	* Step50 Validation of the Object object Device.WiFi.Radio.10100.MCS via WebPa *
	* Step51 Verification of the object object Device.WiFi.Radio.10100.TransmitPowerSupported via TR69
	* Step52 * Validation of the Object object Device.WiFi.Radio.10100.TransmitPowerSupported via WebPa
	* Step53 Verification of * the object object Device.WiFi.Radio.10100.TransmitPower via TR69
	* Step54 Validation of the Object object * Device.WiFi.Radio.10100.TransmitPower via WebPa
	* Step55 Verification of the object object * Device.WiFi.Radio.10100.IEEE80211hSupported via TR69
	* Step56 Validation of the Object object * Device.WiFi.Radio.10100.IEEE80211hSupported via WebPa
	* Step57 Verification of the object object * Device.WiFi.Radio.10100.IEEE80211hEnabled via TR69
	* Step58 Validation of the Object object * Device.WiFi.Radio.10100.IEEE80211hEnabled via WebPa
	* Step59 Verification of the object object * Device.WiFi.Radio.10100.RegulatoryDomain via TR69
	* Step60 Validation of the Object object * Device.WiFi.Radio.10100.RegulatoryDomain via WebPa
    * 
    * @author Sathurya Ravi
    * @refactor yamini.s
    * 
    * @param device
    *            device to be used for execution
    * 
    */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1013")

    public void testValidateTR69WiFiRadioFrequencyObjects(Dut device) {

	// stores the test result
	boolean status = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-013";
	// stores the error message
	String message = "";
	// stores the stepNumber
	String stepNumber = "s1";

	try {

	    /** Steps 1 to 30 */
	    verifyRadioFrequencyObjectsForTR69(BroadBandTestConstants.RADIO_24_GHZ_INDEX,
		    BroadBandTestConstants.CONSTANT_1, device, testId);

	    /** Steps 31 to 60 */
	    verifyRadioFrequencyObjectsForTR69(BroadBandTestConstants.RADIO_5_GHZ_INDEX,
		    BroadBandTestConstants.CONSTANT_31, device, testId);

	} catch (Exception exception) {
	    message = exception.getMessage();
	    status = false;
	    LOGGER.error("Exception occured while validating TR69 WiFi Accesspoint objects" + message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, message, true);
	}

    }

    private void verifyRadioFrequencyObjectsForTR69(String index, int stepNumber, Dut device, String testId) {

	String step = "";
	boolean status = false;
	String response = "";
	String message = "";
	String objectName = "";
	String possibleChannels = "";
	List<String> get_param = new ArrayList<String>();

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.PossibleChannels
	 */
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.PossibleChannels via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.PossibleChannels using the TR69 Rest API");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a range of integers ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_POSSIBLECHANNELS
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandTr69Utils.validatePossibleChannels(response);
	    possibleChannels = response;
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + "| Expected should be a range of valid Integers";
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.PossibleChannels against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.PossibleChannels via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.PossibleChannels using the command, \"curl --request GET --url <WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.PossibleChannels' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandTr69Utils.validatePossibleChannels(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ "| Expected should be a range of valid Integers";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.ChannelsInUse
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.ChannelsInUse via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.ChannelsInUse using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.ChannelsInUse");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a list of channels within the range mentioned in step 13. Example, \"1,2,3,11\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	String channelsInuse = "";
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNELSINUSE
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandTr69Utils.checkForValueInRange(possibleChannels, response);
	    channelsInuse = response;
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a range of valid Integers";
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.ChannelsInUse against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.ChannelsInUse via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.ChannelsInUse using the command, \"curl --request GET --url <WEPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.ChannelsInUse' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandTr69Utils.checkForValueInRange(possibleChannels, response);
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ " Expected value should be a range of valid Integers";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.Channel
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.Channel via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.Channel using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.Channel");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a list of channels within the range mentioned in step 14. Example, \"2\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_CHANNEL
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandTr69Utils.checkForValueInRange(channelsInuse, response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a range of valid integer within the range obtained in step 14 ";
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.Channel against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.Channel via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.Channel using the command, \"curl --request GET --url <WEPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.Channel' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandTr69Utils.checkForValueInRange(channelsInuse, response);
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ " Expected value should be a range of valid integer within the range obtained in step 14 ";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.AutoChannelSupported
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.AutoChannelSupported via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.AutoChannelSupported using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL><Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.AutoChannelSupported");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should of type boolean and have a value either true or false ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_AUTOCHANNELSUPPORTED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandCommonUtils.isBoolean(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " | Expeceted: "
		    + BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			    BroadBandTestConstants.FALSE);
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.AutoChannelSupported against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.AutoChannelSupported via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.AutoChannelSupported using the command, \"curl --request GET --url <WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.AutoChannelSupported' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					   b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.isBoolean(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expeceted: "
		+ BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			BroadBandTestConstants.FALSE);
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.AutoChannelEnable
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.AutoChannelEnable via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.AutoChannelEnable using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.AutoChannelEnable");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should of type boolean and have a value either true or false ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_AUTOCHANNELENABLE
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	if (response != null) {

	    status = BroadBandCommonUtils.isBoolean(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " |Expected: "
		    + BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			    BroadBandTestConstants.FALSE);
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.AutoChannelEnable against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.AutoChannelEnable via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.AutoChannelEnable using the command, \"curl --request GET --url <WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.AutoChannelEnable' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"				       b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.isBoolean(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " |Expected: "
		+ BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			BroadBandTestConstants.FALSE);
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.AutoChannelRefreshPeriod
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.AutoChannelRefreshPeriod via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.AutoChannelRefreshPeriod using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.AutoChannelRefreshPeriod");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: The value returned should be a valid unsigned integer");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_AUTOREFRESHPERIOD
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "",
		    response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected values should be a valid unsigned integer";
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.AutoChannelRefreshPeriod against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.AutoChannelRefreshPeriod via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.AutoChannelRefreshPeriod using the command, \"curl --request GET --url <WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.AutoChannelRefreshPeriod' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "",
		response);
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ " |Expected values should be a valid unsigned integer";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.OperatingChannelBandwidth
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.OperatingChannelBandwidth via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.OperatingChannelBandwidth using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.OperatingChannelBandwidth");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: The value returned should be a valid frequency in MHz");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_OPERATINGCHANNELBANDWIDTH
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_MATCHER_OPERATING_FREQUENCY);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid frequency in MHz";
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.OperatingChannelBandwidth against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.OperatingChannelBandwidth via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.OperatingChannelBandwidth using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.OperatingChannelBandwidth' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_MATCHER_OPERATING_FREQUENCY);
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ " Expected value should be a valid frequency in MHz";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.ExtensionChannel
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.ExtensionChannel via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.ExtensionChannel using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.ExtensionChannel");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be either \"AboveControlChannel\" or \"Auto\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_EXTENSIONCHANNEL
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = CommonMethods.isNotNull(response)
		    && BroadBandTestConstants.EXTENSION_CHANNEL_VALUES.contains(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " |Expected: "
		    + BroadBandTestConstants.EXTENSION_CHANNEL_VALUES.toString();
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.ExtensionChannel against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.ExtensionChannel via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.ExtensionChannel using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.ExtensionChannel' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = CommonMethods.isNotNull(response)
		&& BroadBandTestConstants.EXTENSION_CHANNEL_VALUES.contains(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected: "
		+ BroadBandTestConstants.EXTENSION_CHANNEL_VALUES.toString();
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.GuardInterval
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.GuardInterval via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.GuardInterval using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.GuardInterval");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: The value returned should be \"Auto\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_GUARDINTERVAL
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    BroadBandTestConstants.STRING_AUTO, response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " |Expected: "
		    + BroadBandTestConstants.STRING_AUTO;
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.GuardInterval against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.GuardInterval via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.GuardInterval using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.GuardInterval' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		BroadBandTestConstants.STRING_AUTO, response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected: "
		+ BroadBandTestConstants.STRING_AUTO;
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.MCS
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.MCS via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.MCS using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.MCS");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: The value returned should be integer a valid integer");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_MCS
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    // Checks if the response is a valid integer or not. If it is a valid integer it will not throw number
	    // format exception.
	    if (CommonMethods.isNotNull(response)) {
		try {
		    Integer.parseInt(response);
		} catch (NumberFormatException e) {
		    status = false;
		    LOGGER.error("The value returned as response for parameter " + objectName
			    + " is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
			    e);
		}
		status = true;
	    }
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " | Expected value should be a valid integer";
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	}
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.MCS against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.MCS via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.MCS using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.MCS' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	try {
	    Integer.parseInt(response);
	    status = true;
	} catch (NumberFormatException e) {
	    status = false;
	    LOGGER.error("The value returned as response for parameter " + objectName
		    + " is invalid. NumberFormatException has occured while trying to convert the resposne to integer",
		    e);
	}
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ " Expected value should be a valid integer";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.TransmitPowerSupported
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.TransmitPowerSupported via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.TransmitPowerSupported using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.TransmitPowerSupported");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a range of Integers like \"12,25,50,75,100\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	String transmitPowerSupported = "";
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_TRANSMITPOWERSUPPORTED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandTr69Utils.validatePossibleChannels(response);
	    transmitPowerSupported = response;
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a range of valid Integers";
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.TransmitPowerSupported against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.TransmitPowerSupported via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.TransmitPowerSupported using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.TransmitPowerSupported' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandTr69Utils.validatePossibleChannels(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ " Expected value: Expected value should be a range of valid Integers";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.TransmitPower
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.TransmitPower via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.TransmitPower using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.TransmitPower");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a value within the range mentioned in step 13. Example, \"100\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_TRANSMITPOWER
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandTr69Utils.checkForValueInRange(transmitPowerSupported, response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a range of valid Integers";
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.TransmitPower against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.TransmitPower via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.TransmitPower using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.TransmitPower' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandTr69Utils.checkForValueInRange(transmitPowerSupported, response);
	message = "The Webpa response for the parameter " + objectName + ": " + response
		+ " Expected value should be a range of valid Integers";
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.IEEE80211hSupported
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.IEEE80211hSupported via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.IEEE80211hSupported using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.IEEE80211hSupported");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should of type boolean and have a value either true or false ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_IEEE80211HSUPPORTED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandCommonUtils.isBoolean(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " |Expected: "
		    + BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			    BroadBandTestConstants.FALSE);
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.IEEE80211hSupported against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.IEEE80211hSupported via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.IEEE80211hSupported using the command, \"curl --request GET --url '<WEPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.IEEE80211hSupported' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.isBoolean(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected: "
		+ BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			BroadBandTestConstants.FALSE);
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.IEEE80211hEnabled
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.IEEE80211hEnabled via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.IEEE80211hEnabled using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.IEEE80211hEnabled");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should of type boolean and have a value either true or false ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_IEEE80211HENABLED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandCommonUtils.isBoolean(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " |Expected: "
		    + BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			    BroadBandTestConstants.FALSE);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	}
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.IEEE80211hEnabled against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.IEEE80211hEnabled via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.IEEE80211hEnabled using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.IEEE80211hEnabled' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandCommonUtils.isBoolean(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected: "
		+ BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.TRUE, " or ",
			BroadBandTestConstants.FALSE);
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Execute Get Parameter on the object Device.WiFi.Radio.{i}.RegulatoryDomain
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Verification of the object Device.WiFi.Radio.{i}.RegulatoryDomain via TR69 ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute Get Parameter value on the object Device.WiFi.Radio.{i}.RegulatoryDomain using the TR69 Rest API ,\"curl --request GET --url <TELESCOPIC URL>/<Model>/<Serial Number>/names=Device.WiFi.Radio.{i}.RegulatoryDomain");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: The value returned should be either \"US\" ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_RADIO_REGULATORYDOMAIN
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);

	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	response = tapEnv.getTR69ParameterValues(device, get_param);
	if (response != null) {

	    status = BroadBandTestConstants.WIFI_REGULATORY_DOMAN.contains(response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response + " | Expected: "
		    + BroadBandTestConstants.WIFI_REGULATORY_DOMAN.toString();
	    LOGGER.info("Actual status: " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual status: " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Validate the TR69 response of Device.WiFi.Radio.{i}.RegulatoryDomain against WebPa response
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber
		+ ": DESCRIPTION: Validation of the Object Device.WiFi.Radio.{i}.RegulatoryDomain via WebPa ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: a) Execute WebPa GET command on the object Device.WiFi.Radio.{i}.RegulatoryDomain using the command, \"curl --request GET --url '<WEBPA URL>/device/mac:<ECM MAC>/config?names=Device.WiFi.Radio.{i}.RegulatoryDomain' --header 'authorization: Bearer <SAT-TOKEN>'");
	LOGGER.info(
		"					    b) Compare the WebPa response with the TR69 response from the above step.");
	LOGGER.info("stepNumber " + stepNumber + ": EXPECTED: Device should return same value for TR69 and WebPa");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	response = tapEnv.executeWebPaCommand(device, objectName);
	status = BroadBandTestConstants.WIFI_REGULATORY_DOMAN.contains(response);
	message = "The Webpa response for the parameter " + objectName + ": " + response + " Expected: "
		+ BroadBandTestConstants.WIFI_REGULATORY_DOMAN.toString();
	LOGGER.info("Actual status: " + message);
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

    }
    
    /**
     * [RDKB-Automation - RDK B Reboot Validation tests using TR69/ACS]
     * <p>
     * Step 1 :
     * </p>
     * <li>Verify that device reboots using TR-181 parameter 'Device.X_CISCO_COM_DeviceControl.RebootDevice' via
     * TR-69/ACS</li>
     * <li>Set the TR-181 parameter TR-181 parameter 'Device.X_CISCO_COM_DeviceControl.RebootDevice' with value 'Device'
     * and data type as "string' via TR-69/ACS</li>
     * 
     * <p>
     * Step 2:
     * </p>
     * <li>Verify that device get rebooted and come up after 4 to 5 minutes</li>
     * <li>Device should reboot and comes up online without any issues. SSH should work.s</li>
     * 
     * <p>
     * Step 3:
     * </p>
     * <li>Verify that devices logs the source of reboot properly.</li>
     * <li>Device should log the source of trigger and logs will be backed up before initiating the reboot. Logs will be
     * available in /nvram/logs/ or /nvram2/logs/</li>
     * 
     * <p>
     * Step 4:
     * </p>
     * <li>Verify that device logs the last reboot reason.</li>
     * <li>Device should log the last reboot reason properly.Reboot reason should be something like tr69-reboot.</li>
     * 
     * <p>
     * Step 5:
     * </p>
     * <li>Verify that TR-181 parameter returns the correct reboot reason via TR-69/ACS</li>
     * <li>TR-181 parameter should return the proper last reboot reason via TR-69/ACS</li>
     * 
     * <p>
     * Step 6:
     * </p>
     * <li>Verify that device logs the time required for completing the intermediate stages in BootTime.log</li>
     * <li>Device should logs the time take for completing intermediate stages.</li>
     * 
     * <p>
     * Step 7:
     * </p>
     * <li>Verify that device logs proper log message indicate device up after successful reboot.</li>
     * <li>Device should logs proper log messages to indicate that device is up after successful reboot</li>
     * 
     * 
     * @param device
     * @author Arun V S
     * @refactor Athira
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-TR69-RBT-1002")
    public void testToVerifyRdkbRebootUsingTr69AcsCommand(Dut device) {

	LOGGER.debug("STARTED TESTCASE: TC-RDKB-TR69-RBT-1002");

	/** Variable for holding execution status */
	boolean status = false;

	/** Status of TR69 set */
	boolean tr69SetStatus = false;

	/** Variable for test case ID */
	String testCaseId = "TC-RDKB-REBOOT-002";

	/** Test step */
	String stepNumber = "s1";

	/** Variable for holding logger message for each step */
	String message = "Unable to reboot the device using TR-181 parater 'Device.X_CISCO_COM_DeviceControl.RebootDevice' by setting value as 'Device'";
	try {

	    LOGGER.info("*************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify that  device reboots using TR-181 parameter 'Device.X_CISCO_COM_DeviceControl.RebootDevice' via TR-69/ACS.");
	    LOGGER.info(
		    "STEP 1 : ACTION : Reboot device using TR-181 parameter 'Device.X_CISCO_COM_DeviceControl.RebootDevice'");
	    LOGGER.info(
		    "EXPECTED: Should able to set the TR-181 parameter  via TR-69/ACS. And the device should reboot automatically");
	    LOGGER.info("*************************************************************************");

	    /**
	     * Setting to TR69/ACS execution
	     * 
	     */
	    LOGGER.info("Setting isWebPA - false");
	    System.setProperty("isWebPA", "false");

	    /*
	     * Due to bootup logs, it takes more time for searching in buffer. So clearing buffer and start buffering
	     * the trace.
	     */
	    tapEnv.cleanupTraceBuffer(device);

	    /**
	     * Rebooting the box using TR-181 parameter 'Device.X_CISCO_COM_DeviceControl.RebootDevice' via TR-69/ACS
	     * 
	     */
	    tr69SetStatus = BootTimeUtils.rebootDeviceUsingDeviceControlRebootTr69parameter(tapEnv, device);
	    LOGGER.info("TR69 Command execution status - " + tr69SetStatus);

	    /**
	     * Retrieve TR-69 logs to identify the TR-69 reboot using trace provider, After reboot, trace get restarted
	     * which clears the buffered logs.
	     */
	    boolean rebootTr69DeviceLogStatus = BootTimeUtils.verifyRebootReasonFromTr69Logs(tapEnv, device);

	    LOGGER.info("TR-69 Reboot Log 'RDKB_REBOOT : RebootDevice triggered from TR69 with value Device' Status = "
		    + rebootTr69DeviceLogStatus);

	    long timeToRebootDevice = BroadBandTestConstants.TWO_MINUTE_IN_MILLIS;
	    tapEnv.waitTill(timeToRebootDevice);

	    /** Checking if the box rebooted successfully. Make sure that the ip is not accessible */
	    boolean stbAccessible = CommonMethods.isSTBAccessible(device);
	    if (stbAccessible) {
		message = "STEP 1: ACTUAL :TR69 Command execution status - " + tr69SetStatus
			+ " and Device is still up after executing tr69 paramter to reboot the device";
		status = false;
	    } else {
		LOGGER.info("STEP 1: ACTUAL :Successfully initiated reboot on device using TR69/ACS");
		status = true;
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status && tr69SetStatus, message, true);

	    LOGGER.info("*************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify that  device get  rebooted and come up after 4 to 5 minutes");
	    LOGGER.info("STEP 2 : ACTION : Verify if device accessible or not");
	    LOGGER.info(
		    "STEP 2 : EXPECTED: Device should reboot and comes up online without any issues. SSH should work.");
	    LOGGER.info("*************************************************************************");

	    stepNumber = "s2";
	    status = false;
	    message = "Device is not coming up after rebooting using 'RDKB_REBOOT : RebootDevice triggered from TR69'";
	    status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
	    LOGGER.info("STEP 2: ACTUAL : IP acquisition after bootup status - " + status);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, true);

	    LOGGER.info("*************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify that devices logs the source of reboot properly.");
	    LOGGER.info("STEP 3 : ACTION : Verify if device log the source of trigger");
	    LOGGER.info(
		    "STEP 3 : EXPECTED: Device should log the source of trigger 'RDKB_REBOOT : RebootDevice triggered from TR69' before initiating the reboot.");
	    LOGGER.info("*************************************************************************");

	    stepNumber = "s3";
	    /**
	     * 
	     * Getting the reboot logs status from tr69 logs before reboot
	     * 
	     */
	    status = rebootTr69DeviceLogStatus;
	    LOGGER.info(
		    "STEP 3: ACTUAL : TR-69 Reboot Log 'RDKB_REBOOT : RebootDevice triggered from TR69 with value Device' Status = "
			    + rebootTr69DeviceLogStatus);
	    message = "Unable to retrieve Telemetry marker 'RDKB_REBOOT : RebootDevice triggered from TR69 with value' "
		    + "log message from logs in TR69log";
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, false);

	    /**
	     * Waiting for 5 minutes to make sure the required services are up and running in the box
	     * 
	     */
	    LOGGER.info("Waiting for 5 minutes to make sure the required services are up and running in the box");
	    tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);
	    LOGGER.info("Wait completed.");

	    LOGGER.info("*************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify that  device logs the last reboot reason.");
	    LOGGER.info(
		    "STEP 4 : ACTION : retrieve telemetry marker 'Received reboot_reason as:tr069-reboot' from WEBPAlog or PARODUSlog");
	    LOGGER.info(
		    "STEP 4 : EXPECTED: Device should log the last reboot reason properly.Reboot reason should be something like tr69-reboot.");
	    LOGGER.info("*************************************************************************");

	    stepNumber = "s4";
	    status = false;
	    message = "Unable to retrieve telemetry marker 'Received reboot_reason as:tr069-reboot' to indicate the tr69 reboot "
		    + "from WEBPAlog or PARODUSlog";
	    status = BootTimeUtils.verifyRebootReasonFromWebpaOrPardusLogs(tapEnv, device);
	    if (status)
		LOGGER.info("STEP 4: ACTUAL : Device logged the last reboot reason properly ");
	    else
		LOGGER.info("STEP 4: ACTUAL :  " + message);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, false);

	    LOGGER.info("*************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION :  Verify that TR-181 parameter returns the correct reboot reason via TR-69/ACS. ");
	    LOGGER.info(
		    "STEP 5 : ACTION : retrieve the last reboot reason as 'Received reboot_reason as:tr069-reboot' using TR-181 parameter - 'Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason'");
	    LOGGER.info(
		    "STEP 5 : EXPECTED: Execute TR-69/ACS command to retrieve the last reboot reason using TR-181 parameter 'Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason'");
	    LOGGER.info("*************************************************************************");

	    stepNumber = "s5";
	    status = false;
	    message = "Unable to retrieve the last reboot reason as tr069-reboot using TR-181 parameter - 'Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason'";
	    status = BootTimeUtils.verifyRebootResonFromWebPaCommand(tapEnv, device);

	    if (status)
		LOGGER.info("STEP 5: ACTUAL : Device logged the last reboot reason properly ");
	    else
		LOGGER.info("STEP 5: ACTUAL :  " + message);

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, false);

	    LOGGER.info("*************************************************************************");

	    LOGGER.info("*************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Check device logs for the time required for completing the intermediate stages in BootTime.log. ");
	    LOGGER.info("STEP 6 : ACTION : Check bootup time from bootlog");
	    LOGGER.info("STEP 6 : EXPECTED:  Device should logs the time taken for completing intermediate stages.");
	    LOGGER.info("*************************************************************************");

	    stepNumber = "s6";
	    status = false;
	    message = "Bootup log verification failed.";

	    try {
		BootTimeUtils.verifyBootupTimeFromBootLog(tapEnv, device);
		status = true;
	    } catch (TestException testException) {
		message = testException.getMessage();
		status = false;
	    }
	    if (status)
		LOGGER.info("STEP 6: ACTUAL : Device logged the time taken for completing intermediate stages. ");
	    else
		LOGGER.info("STEP 6: ACTUAL :  " + message);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, false);

	    LOGGER.info("*************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify that device logs proper log message which indicate device up after successful reboot.  ");
	    LOGGER.info("STEP 7 : ACTION : Check ConsoleLog");
	    LOGGER.info(
		    "STEP 7 : EXPECTED:  Device should logs proper log messages to indicate that device is up after successful reboot");
	    LOGGER.info("*************************************************************************");

	    stepNumber = "s7";
	    status = false;
	    message = "Unable to retrieve the telemetry marker 'RDKB_REBOOT: Device is up after reboot' from Consolelog or ArmConsolelog";
	    status = BootTimeUtils.verifyDeviceIsUpAfterRebootTelemetryMarkerFromConsoleLogs(tapEnv, device);
	    if (status)
		LOGGER.info(
			"STEP 7: ACTUAL : Retrieved the telemetry marker 'RDKB_REBOOT: Device is up after reboot' from Consolelog or ArmConsolelog ");
	    else
		LOGGER.info("STEP 7: ACTUAL :  " + message);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, message, false);

	} catch (TestException e) {
	    LOGGER.error("Test Failure - " + e);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, false, message, false);
	}
	LOGGER.debug("ENDED TESTCASE: TC-RDKB-TR69-RBT-1002");
    }
    
    
    /**
     * Step1: Verification of the object Device.WiFi.SSID.10001.Stats.BytesSent Step2: Verification of the object
     * Device.WiFi.SSID.10001.Stats.BytesReceived Step3: Verification of the object
     * Device.WiFi.SSID.10001.Stats.PacketsSent Step4: Verification of the object
     * Device.WiFi.SSID.10001.Stats.PacketsReceived Step5: Verification of the object
     * Device.WiFi.SSID.10001.Stats.ErrorsSent Step6: Verification of the object
     * Device.WiFi.SSID.10001.Stats.ErrorsReceived Step7: Verification of the object
     * Device.WiFi.SSID.10001.Stats.UnicastPacketsSent Step8: Verification of the object
     * Device.WiFi.SSID.10001.Stats.UnicastPacketsReceived Step9: Verification of the object
     * Device.WiFi.SSID.10001.Stats.DiscardPacketsSent Step10: Verification of the object
     * Device.WiFi.SSID.10001.Stats.DiscardPacketsReceived Step11: Verification of the object
     * Device.WiFi.SSID.10001.Stats.MulticastPacketsSent Step12: Verification of the object
     * Device.WiFi.SSID.10001.Stats.MulticastPacketsReceived Step13: Verification of the object
     * Device.WiFi.SSID.10001.Stats.BroadcastPacketsSent Step14: Verification of the object
     * Device.WiFi.SSID.10001.Stats.BroadcastPacketsReceived Step15: Verification of the object
     * Device.WiFi.SSID.10001.Stats.UnknownProtoPacketsReceived Step16: Verification of the object
     * Device.WiFi.SSID.10001.Stats.RetransCount Step17: Verification of the object
     * Device.WiFi.SSID.10001.Stats.FailedRetransCount Step18: Verification of the object
     * Device.WiFi.SSID.10001.Stats.RetryCount Step19: Verification of the object
     * Device.WiFi.SSID.10001.Stats.MultipleRetryCount Step20: Verification of the object
     * Device.WiFi.SSID.10001.Stats.ACKFailureCount Step21: Verification of the object
     * Device.WiFi.SSID.10001.Stats.AggregatedPacketCount Step22: Verification of the object
     * Device.WiFi.SSID.10101.Stats.BytesSent Step23: Verification of the object
     * Device.WiFi.SSID.10101.Stats.BytesReceived Step24: Verification of the object
     * Device.WiFi.SSID.10101.Stats.PacketsSent Step25: Verification of the object
     * Device.WiFi.SSID.10101.Stats.PacketsReceived Step26: Verification of the object
     * Device.WiFi.SSID.10101.Stats.ErrorsSent Step27: Verification of the object
     * Device.WiFi.SSID.10101.Stats.ErrorsReceived Step28: Verification of the object
     * Device.WiFi.SSID.10101.Stats.UnicastPacketsSent Step29: Verification of the object
     * Device.WiFi.SSID.10101.Stats.UnicastPacketsReceived Step30: Verification of the object
     * Device.WiFi.SSID.10101.Stats.DiscardPacketsSent Step31: Verification of the object
     * Device.WiFi.SSID.10101.Stats.DiscardPacketsReceived Step32: Verification of the object
     * Device.WiFi.SSID.10101.Stats.MulticastPacketsSent Step33: Verification of the object
     * Device.WiFi.SSID.10101.Stats.MulticastPacketsReceived Step34: Verification of the object
     * Device.WiFi.SSID.10101.Stats.BroadcastPacketsSent Step35: Verification of the object
     * Device.WiFi.SSID.10101.Stats.BroadcastPacketsReceived Step36: Verification of the object
     * Device.WiFi.SSID.10101.Stats.UnknownProtoPacketsReceived Step37: Verification of the object
     * Device.WiFi.SSID.10101.Stats.RetransCount Step38: Verification of the object
     * Device.WiFi.SSID.10101.Stats.FailedRetransCount Step39: Verification of the object
     * Device.WiFi.SSID.10101.Stats.RetryCount Step40: Verification of the object
     * Device.WiFi.SSID.10101.Stats.MultipleRetryCount Step41: Verification of the object
     * Device.WiFi.SSID.10101.Stats.ACKFailureCount Step42: Verification of the object
     * Device.WiFi.SSID.10101.Stats.AggregatedPacketCount
     * 
     * @author Sathurya Ravi
     * @refactor Alan_Bivera
     * 
     * @param Dut
     *            device to be used for execution
     * 
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI, BroadBandTestGroup.TR69 })
    @TestDetails(testUID = "TC-RDKB-TR69-1023")

    public void testValidateTR69WiFiSsidStatsObjects(Dut device) {

	// stores the test result
	boolean status = false;
	// stores the test case id
	String testId = "TC-RDKB-TR69-023";
	// stores the error message
	String message = "";
	// stores the stepNumber
	String stepNumber = "s1";

	try {

	    /** Steps 1 to 21 */
	    verifySsidStatsObjects(BroadBandTestConstants.WIFI_24_GHZ_INDEX, testId, device,
		    BroadBandTestConstants.CONSTANT_1);

	    /** Steps 22 to 42 */
	    verifySsidStatsObjects(BroadBandTestConstants.WIFI_5_GHZ_INDEX, testId, device,
		    BroadBandTestConstants.CONSTANT_22);

	} catch (Exception exception) {
	    message = exception.getMessage();
	    LOGGER.error("Exception occured while validating private wifi stats objects " + message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNumber, status, message, true);
	}
    }

    private void verifySsidStatsObjects(String index, String testId, Dut device, int stepNumber) {
	String step = "";
	String response = "";
	String objectName = "";
	boolean status = false;
	// List<Parameter> parameterList = null;
	String message = "";
	List<String> get_param = new ArrayList<String>();

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.BytesSent
	 */
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.BytesSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.BytesSent using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.BytesSent\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_BYTESENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.BytesReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.BytesReceived  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.BytesReceived using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.BytesReceived\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_BYTESRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.PacketsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.PacketsSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.PacketsSent using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.PacketsSent\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_PACKETSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.PacketsReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.PacketsReceived  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.PacketsReceived using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.PacketsReceived\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_PACKETSRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.ErrorsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.ErrorsSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.ErrorsSent using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.ErrorsSent\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_ERRORSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.ErrorsReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.ErrorsReceived  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.ErrorsReceived using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.ErrorsReceived\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_ERRORSRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.UnicastPacketsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.UnicastPacketsSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.UnicastPacketsSent using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.UnicastPacketsSent\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_UNICASTPACKETSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.UnicastPacketsReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.UnicastPacketsReceived  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.UnicastPacketsReceived using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.UnicastPacketsReceived\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_UNICASTPACKETSRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.DiscardPacketsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.DiscardPacketsSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.DiscardPacketsSent using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.DiscardPacketsSent\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_DISCARDPACKETSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.DiscardPacketsReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.DiscardPacketsReceived  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.DiscardPacketsReceived using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.DiscardPacketsReceived\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_DISCARDPACKETSRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.MulticastPacketsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.MulticastPacketsSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.MulticastPacketsSent using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.MulticastPacketsSent\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_MULTICASTPACKETSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.MulticastPacketsReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.MulticastPacketsReceived   ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.MulticastPacketsReceived  using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.MulticastPacketsReceived \" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_MULTICASTPACKETSRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.BroadcastPacketsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.BroadcastPacketsSent   ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.BroadcastPacketsSent  using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.BroadcastPacketsSent \" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_BROADCASTPACKETSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.BroadcastPacketsReceived
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.BroadcastPacketsReceived   ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.BroadcastPacketsReceived  using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.BroadcastPacketsReceived \" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_BROADCASTPACKETSRECEIVED
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.UnknownProtoPacketsSent
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.UnknownProtoPacketsSent  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.UnknownProtoPacketsSent using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.UnknownProtoPacketsSent\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_UNKNOWNPROTOCOLPACKETSSENT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.RetransCount
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.RetransCount  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.RetransCount using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.RetransCount\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_RETRANSCOUNT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.FailedRetransCount
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.FailedRetransCount  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.FailedRetransCount using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.FailedRetransCount\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_FAILEDRETRANSCOUNT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.RetryCount
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.RetryCount  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.RetryCount using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.RetryCount\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_RETRYCOUNT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.MultipleRetryCount
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.MultipleRetryCount  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.MultipleRetryCount using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.MultipleRetryCount\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_MULTIPLERETRYCOUNT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);
	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.ACKFailureCount
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.ACKFailureCount  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.ACKFailureCount using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.ACKFailureCount\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_ACKFAILURECOUNT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);

	/*
	 * Verification of the object Device.WiFi.SSID.{i}.Stats.AggregatedPacketCount
	 */
	++stepNumber;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("stepNumber " + stepNumber + ": DESCRIPTION: Verification of the  object Device.WiFi.SSID." + index
		+ ".Stats.AggregatedPacketCount  ");
	LOGGER.info("stepNumber " + stepNumber
		+ ": ACTION: 1. Execute Get Parameter value on the object Device.WiFi.SSID." + index
		+ ".Stats.AggregatedPacketCount using the TR69 Rest API "
		+ ",\"curl --request GET --url https://<url>/telescope/api/<Model>/<Serial Number>/names=Device.WiFi.SSID."
		+ index + ".Stats.AggregatedPacketCount\" .");
	LOGGER.info("stepNumber " + stepNumber
		+ ": EXPECTED: The value returned should be a string and it should be a valid unsigned integer ");
	LOGGER.info("**********************************************************************************");
	step = "s" + stepNumber;
	status = false;
	objectName = BroadBandWebPaConstants.WEBPA_DEVICE_WIFI_SSID_STATS_AGGREGATEDPACKETCOUNT
		.replace(BroadBandTestConstants.TR181_NODE_REF, index);
	get_param.clear();
	get_param = BroadBandTr69Utils.getParameterForTr69Get(objectName);
	if (get_param != null) {
	    response = tapEnv.getTR69ParameterValues(device, get_param);
	    status = CommonMethods.isNotNull(response) && BroadBandCommonUtils
		    .compareValues(BroadBandTestConstants.CONSTANT_INT_UNSIGNED_INC_ZERO, "", response);
	    message = "The TR69 response for the parameter " + objectName + ": " + response
		    + " |Expected value should be a valid unsigned integer.";
	    LOGGER.info("Actual Status : " + message);
	} else {
	    message = "The response for Get Parameter Value on " + objectName + " is null.";
	    LOGGER.error("Actual Status : " + message);
	}
	tapEnv.updateExecutionStatus(device, testId, step, status, message, false);
    }

    
}